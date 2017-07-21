# Author: Sean Hendryx
# Graduate Student
# Date: October 17th, 2016

# Script Adapted from:
# Clayton T. Morrison, 13 September 2015
# Based on cv_demo.m
# From A First Course in Machine Learning, Chapter 1.
# Simon Rogers, 31/10/11 [simon.rogers@glasgow.ac.uk]


# NOTE:
# When summarizing log errors, DO NOT take the mean of the log
# instead, first take the mean of the errors, then take the log of the mean

import numpy
import matplotlib.pyplot as plt


def main():
    """

    """
    #MAIN FUNCTION:

    #First read in data wtih structural parameter and mass
    filePath = "/Users/seanhendryx/DATA/ecosystemAllometry/Stochastic_Mass_CA.csv"
    # !! NOTE THAT DATATYPE IN genfromtxt() HAS BEEN EXPLICITLY SPECIFIED AS FLOAT THEREFORE STRINGS WILL BE NAN
    data = read_data(filePath)

    # !! MAKE SURE x AND t ARE THE COLUMNS THEY ARE SUPPOSED TO BE !!:
    #Extract independent variable (Canopy Area)
    x = data[1:, 1]  # extract x (slice first column)
    #And Extract response variable (Mass)
    t = data[1:, 2]  # extract t (slice second column)


    #Run 10-fold cros-validation to determine optimal polynomial order from 0 to 8
    K = 10
    maxOrder = 7
    #Run cross validation multiple times and record best_poly and min_mean_log_cv_loss:
    # Runs cv numTrials times
    numTrials = 1
    cvTrials = numpy.zeros((numTrials, 3))
    for i in range(numTrials):
        best_poly, min_mean_log_cv_loss = run_cv(K = K, maxorder = maxOrder, x = x, t = t, cvType = "10 Fold", randomize_data=True, title='CV', plotter=False)
        cvTrials[i,:] = i, best_poly, min_mean_log_cv_loss
    print "{0}-fold CV trials (trial number, best polygon order from CV, Min mean CV loss): ".format(K), "\n",  cvTrials
    numpy.savetxt("/Users/seanhendryx/DATA/ecosystemAllometry/CVTrials.csv", cvTrials, fmt='%.18e', delimiter=',', newline='\n', header='trial, bestPolyOrder, minMeanCVLoss', footer='', comments='# ')

    """
    #LOOCV:
    K = x.shape[0]
    maxOrder = 7
    #Run cross validation multiple times and record best_poly and min_mean_log_cv_loss:
    # Runs cv numTrials times
    numTrials = 1
    cvTrials = numpy.zeros((numTrials, 3))
    for i in range(numTrials):
        best_poly, min_mean_log_cv_loss = run_cv(K = K, maxorder = maxOrder, x = x, t = t, cvType = "LOO", randomize_data=True, title='CV', plotter=False)
        cvTrials[i,:] = i, best_poly, min_mean_log_cv_loss
    print "{0}-fold CV trials (trial number, best polygon order from CV, Min mean CV loss): ".format(K), cvTrials
    numpy.savetxt("/Users/seanhendryx/DATA/ecosystemAllometry/LOOCVTrials.csv", cvTrials, fmt='%.18e', delimiter=',', newline='\n', header='trial, bestPolyOrder, minMeanCVLoss', footer='', comments='# ')
    """

    modelOrder = numpy.average(cvTrials[:,1])
    modelOrder = int(round(modelOrder))
    print "Best-fit model order according to 10-folds CV:\n", modelOrder

    # Report the best-fit model parameters for the best model order according to LOOCV, and plot this model with the data.
    plot_data(x, t)

    #Get best-fit model parameters:
    w = fitpoly(x, t, modelOrder)
    plot_model(x, w, color='r')

    plt.show()

# -------------------------------------------------------------------------
# Utilities

# The following permutation tools are used below in run_cv() as a convenience
# for randomly sorting the order of the data (the indices of the input data)

def random_permutation_matrix(n):
    """
    Generate a permutation matrix: an NxN matrix in which each row
    and each column has only one 1, with 0's everywhere else.
    See: https://en.wikipedia.org/wiki/Permutation_matrix
    :param n: size of the square permutation matrix
    :return: NxN permutation matrix
    """
    rows = numpy.random.permutation(n)
    cols = numpy.random.permutation(n)
    m = numpy.zeros((n, n))
    for r, c in zip(rows, cols):
        m[r][c] = 1
    return m


def permute_rows(X, P=None):
    """
    Permute the rows of a 2-d array (matrix) according to
    permutation matrix P.
    If no P is provided, a random permutation matrix is generated.
    :param X: 2-d array
    :param P: Optional permutation matrix; default=None
    :return: new version of X with rows permuted according to P
    """
    if P is None:
        P = random_permutation_matrix(X.shape[0])
    return numpy.dot(P, X)


def permute_cols(X, P=None):
    """
    Permute the columns of a 2-d array (matrix) according to
    permutation matrix P.
    If no P is provided, a random permutation matrix is generated.
    :param X: 2-d array
    :param P: Optional permutation matrix; default=None
    :return: new version of X with columns permuted according to P
    """
    if P is None:
        P = random_permutation_matrix(X.shape[0])
    return numpy.dot(X, P)


def test_permutation_math():
    # permutation matrix
    # will permute rows from 0, 1, 2 to 1, 2, 0
    # will permute cols from 0, 1, 2 to 2, 0, 1
    P = numpy.array([[0, 1, 0],
                     [0, 0, 1],
                     [1, 0, 0]])
    # original matrix
    m = numpy.array([[1, 2, 3],
                     [4, 5, 6],
                     [7, 8, 9]])
    # rows permuted to 1, 2, 0 by left-multiplying P x m
    mr = numpy.array([[4, 5, 6],
                      [7, 8, 9],
                      [1, 2, 3]])
    # cols permuted to 2, 0, 1 by right-multiplying m x P
    mc = numpy.array([[3, 1, 2],
                      [6, 4, 5],
                      [9, 7, 8]])

    # left-multiply by P to permute rows
    mrowp = numpy.dot(P, m)
    assert numpy.array_equal(mr, mrowp)

    # right-multiply by P to permute columns
    mcolp = numpy.dot(m, P)
    assert numpy.array_equal(mc, mcolp)

    print 'TEST permutation_math PASSED'

test_permutation_math()


# -------------------------------------------------------------------------
# Utilities from fitpoly

def read_data(filepath, d=',', dataType=(float)):
    """ returns an numpy.array of the data """
    return numpy.genfromtxt(filepath, delimiter=d, dtype=dataType)


def plot_data(x, t):
    """
    Plot single input feature x data with corresponding response
    values t as a scatter plot
    :param x: sequence of 1-dimensional input data features
    :param t: sequence of 1-dimensional responses
    :return: None
    """
    plt.figure()  # Create a new figure object for plotting
    plt.scatter(x, t, edgecolor='b', color='w', marker='o')
    plt.xlabel('x')
    plt.ylabel('t')
    plt.title('Data')
    plt.pause(.1)  # required on some systems so that rendering can happen


def plot_model(x, w, color='r'):
    """
    Plot the curve for an n-th order polynomial model:
        t = w0*x^0 + w1*x^1 + w2*x^2 + ... wn*x^n
    This works by creating a set of x-axis (plotx) points and
    then use the model parameters w to determine the corresponding
    t-axis (plott) points on the model curve.
    :param x: sequence of 1-dimensional input data features
    :param w: n-dimensional sequence of model parameters: w0, w1, w2, ..., wn
    :param color: matplotlib color to plot model curve
    :return: the plotx and plott values for the plotted curve
    """
    # NOTE: this assumes a figure() object has already been created.
    plotx = numpy.linspace(min(x) - 0.25, max(x) + 0.25, 100)
    plotX = numpy.zeros((plotx.shape[0], w.size))
    for k in range(w.size):
        plotX[:, k] = numpy.power(plotx, k)
    plott = numpy.dot(plotX, w)
    plt.plot(plotx, plott, color=color, linewidth=2)
    plt.pause(.1)  # required on some systems so that rendering can happen
    return plotx, plott


# -------------------------------------------------------------------------
# Synthetic data generation

def generate_synthetic_data(N, w, xmin=-5, xmax=5, sigma=150):
    """
    Generate some synthetic data
    :param N: Number of sample points
    :param w: numpy array (1d) representing generating model parameters
    :param xmin: x minimum
    :param xmax: x maximum
    :param sigma: standard deviation
    :return:
    """
    # generate N random input x points between [xmin, xmax]
    x = (xmax - xmin) * numpy.random.rand(N) + xmin

    # generate response with Gaussian random noise
    X = numpy.zeros((x.size, w.size))
    for k in range(w.size):
        X[:, k] = numpy.power(x, k)
    t = numpy.dot(X, w) + sigma * numpy.random.randn(x.shape[0])

    return x, t


def plot_synthetic_data(x, t, w, filepath=None):
    plot_data(x, t)
    plt.title('Plot of synthetic data; green curve is original generating function')
    plot_model(x, w, color='g')
    if filepath:
        plt.savefig(filepath, format='pdf')


# -------------------------------------------------------------------------
def plot_cv_results1(polys, train_loss, cv_loss, cvType, log_scale_p=False):
    """
    Helper function to plot the results of cross-validation
    :param lambs: lambda values used for x values
    :param train_loss:
    :param cv_loss:
    :param cvType: String put into plot title indicating k-fold with number of folds (k) or LOOCV
    :param log_scale_p:
    :return:
    """

    #Make font:
    mpl.rc('font',family='Avenir')
    afont = {'fontname':'Avenir'}


    fig = plt.figure()
    ax = fig.add_subplot(111)
    ttl = ax.title
    ttl.set_position([.5, 1.01])

    if log_scale_p:
        plt.title('Log-scale Mean Squared Error Loss', **afont)
        ylabel = 'Log MSE Loss'
    else:
        plt.title('Mean Squared Error Loss', **afont)
        ylabel = 'MSE Loss'

    x = polys

    #My plots for lamda optimization:
    ax.set_xlabel(r'$p$')
    ax.set_ylabel(ylabel, **afont)
    ax.xaxis.labelpad = 10
    ax.xaxis.label.set_size(19)
    ax.plot(x, train_loss)
    ax.plot(x, cv_loss)
    ax.legend(['Training Loss', 'CV Loss'], loc='best')

    # End plot_cv_results()
####################################################################################################################################################################################

def plot_cv_results(train_loss, cv_loss, cvType, log_scale_p=False):
    """
    Helper function to plot the results of cross-validation
    :param train_loss:
    :param cv_loss:
    :param cvType: String indicating k-fold with number of folds (k) or LOOCV
    :param log_scale_p:
    :return:
    """

    plt.figure()
    if log_scale_p:
        plt.title('Log-scale Mean Square Error Loss')
        ylabel = 'Log MSE Loss'
    else:
        plt.title('Mean Squared Error Loss')
        ylabel = 'MSE Loss'

    x = numpy.arange(0, train_loss.shape[0])

    # put y-axis on same scale for all plots
    # min_ylim = min(list(train_loss) + list(cv_loss) + list(ind_loss))
    min_ylim = min(min(train_loss), min(cv_loss))
    min_ylim = int(numpy.floor(min_ylim))
    max_ylim = max(list(train_loss) + list(cv_loss))
    max_ylim = int(numpy.ceil(max_ylim))

    plt.subplot(131)
    plt.plot(x, train_loss, linewidth=2)
    plt.xlabel('Model Order')
    plt.ylabel(ylabel)
    plt.title('Train Loss')
    plt.pause(.1) # required on some systems so that rendering can happen
    plt.ylim(min_ylim, max_ylim)

    plt.subplot(132)
    plt.plot(x, cv_loss, linewidth=2)
    plt.xlabel('Model Order')
    plt.ylabel(ylabel)
    plt.title('{0} CV Loss'.format(cvType))
    plt.pause(.1) # required on some systems so that rendering can happen
    plt.ylim(min_ylim, max_ylim)

    """
    plt.subplot(133)
    plt.plot(x, ind_loss, linewidth=2)
    plt.xlabel('Model Order')
    plt.ylabel(ylabel)
    plt.title('Independent Test Loss')
    plt.pause(.1) # required on some systems so that rendering can happen
    plt.ylim(min_ylim, max_ylim)
    """

    plt.subplots_adjust(right=0.95, wspace=0.4)
    plt.draw()


# -------------------------------------------------------------------------

def run_cv( K, maxorder, x, t, cvType, randomize_data=False, title='CV', plotter=True ):
    """

    :param K: Number of folds
    :param maxorder: Integer representing the highest polynomial order
    :param x: input data (1d observations)
    :param t: target data
    :param cvType: String indicating k-fold with number of folds (k) or LOOCV
    :param randomize_data: Boolean (default False) whether to randomize the order of the data
    :param title: Title for plots of results
    :param plotter: Boolean to control whether or not plots are generated
    :return: best_poly, , min_mean_log_cv_loss
    """

    N = x.shape[0]  # number of data points

    # Use when you want to ensure the order of the data has been
    # randomized before splitting into folds
    # Note that in the simple demo here, the data is already in
    # random order.  However, if you use this function more generally
    # for new data, you may need to ensure you're randomizing the
    # order of the data!
    if randomize_data:
        # use the same permutation P on both x and t, otherwise they'll
        # each be in different orders!
        P = random_permutation_matrix(x.size)
        x = permute_rows(x, P)
        t = permute_rows(t, P)

    # Storage for the design matrix used during training
    # Here we create the design matrix to hold the maximum sized polynomial order
    # When computing smaller model polynomial orders below, we'll just use
    # the first 'k+1' columns for the k-th order polynomial.  This way we don't
    # have to keep creating new design matrix arrays.
    X = numpy.zeros((x.shape[0], maxorder + 1))
    # Design matrix for independent test data
    #testX = numpy.zeros((testx.shape[0], maxorder + 1))

    # Create approximately equal-sized fold indices
    # These correspond to indices in the design matrix (X) rows
    # (where each row represents one training input x)
    fold_indices = map(lambda x: int(x), numpy.linspace(0, N, K + 1))
    # storage for recording loss across model polynomial order
    # rows = fold loss (each row is the loss for one fold)
    # columns = model polynomial order
    cv_loss = numpy.zeros((K, maxorder + 1))     # cross-validation loss
    train_loss = numpy.zeros((K, maxorder + 1))  # training loss
    #ind_loss = numpy.zeros((K, maxorder + 1))    # independent test loss

    # iterate over model polynomial orders
    for p in range(maxorder + 1):
        # Augment the input data by the polynomial model order
        # E.g., 2nd-order polynomial model takes input x to the 0th, 1st, and 2nd power
        X[:, p] = numpy.power(x, p)

        # ... do the same for the independent test data
        #testX[:, p] = numpy.power(testx, p)

        # iterate over folds
        for fold in range(K):
            # Partition the data
            # foldX, foldt contains the data for just one fold being held out
            # trainX, traint contains all other data
            foldX = X[fold_indices[fold]:fold_indices[fold+1], 0:p+1]
            foldt = t[fold_indices[fold]:fold_indices[fold+1]]

            # safely copy the training data (so that deleting doesn't remove the original
            trainX = numpy.copy(X[:, 0:p + 1])
            # remove the fold x from the training set
            trainX = numpy.delete(trainX, numpy.arange(fold_indices[fold], fold_indices[fold + 1]), 0)

            # safely copy the training data (so that deleting doesn't remove the original
            traint = numpy.copy(t)
            # remove the fold t from the training set
            traint = numpy.delete(traint, numpy.arange(fold_indices[fold], fold_indices[fold + 1]), 0)

            # find the least mean squares fit to the training data
            ### YOUR CODE HERE ###
            # Calculate w vector (as a numpy.array):
            w = numpy.dot(numpy.dot(numpy.linalg.inv(numpy.dot(numpy.transpose(trainX),trainX)), numpy.transpose(trainX)), traint)

            # calculate and record the mean squared losses

            train_pred = numpy.dot(trainX, w)  # model predictions on training data
            train_loss[fold, p] = numpy.mean(numpy.power(train_pred - traint, 2))

            fold_pred = numpy.dot(foldX, w)  # model predictions on held-out fold
            cv_loss[fold, p] = numpy.mean(numpy.power(fold_pred - foldt, 2))

            #ind_pred = numpy.dot(testX[:, 0:p + 1], w)   # model predictions on independent test data
            #ind_loss[fold, p] = numpy.mean(numpy.power(ind_pred - testt, 2))

    # The loss values can get quite large, so take the log for display purposes

    # Ensure taking log of the mean (not mean of the log!)
    mean_train_loss = numpy.mean(train_loss, 0)
    mean_cv_loss = numpy.mean(cv_loss, 0)
    #mean_ind_loss = numpy.mean(ind_loss, 0)

    log_mean_train_loss = numpy.log(mean_train_loss)
    log_mean_cv_loss = numpy.log(mean_cv_loss)
    #log_mean_ind_loss = numpy.log(mean_ind_loss)

    print '\n----------------------\nResults for {0}'.format(title)
    print 'log_mean_train_loss:\n{0}'.format(log_mean_train_loss)
    print 'log_mean_cv_loss:\n{0}'.format(log_mean_cv_loss)
    #print 'log_mean_ind_loss:\n{0}'.format(log_mean_ind_loss)

    min_mean_log_cv_loss = min(log_mean_cv_loss)
    # TODO: has to be better way to get the min index...
    best_poly = [i for i, j in enumerate(log_mean_cv_loss) if j == min_mean_log_cv_loss][0]

    print 'minimum mean_log_cv_loss of {0} for order {1}'.format(min_mean_log_cv_loss, best_poly)

    # Plot log scale loss results
    if plotter:
        plot_cv_results1(log_mean_train_loss, log_mean_cv_loss, cvType, log_scale_p=True)

    # Uncomment to plot direct-scale mean loss results
    # plot_cv_results(mean_train_loss, mean_cv_loss, mean_ind_loss, log_scale_p=False)

    return best_poly, min_mean_log_cv_loss


# -------------------------------------------------------------------------

def run_demo():
    """
    Top-level script to run the cv demo
    """

    # Parameters for synthetic data model
    # t = x - x^2 + 5x^3 + N(0, sigma)
    w = numpy.array([0, 1, 5, 2])
    # t = x + 5x^2 + 2x^3 + N(0, sigma)
    xmin = -6
    xmax = 6
    sigma = 50

    x, t = generate_synthetic_data(100, w, xmin=xmin, xmax=xmax, sigma=sigma)
    testx, testt, = generate_synthetic_data(1000, w, xmin=xmin, xmax=xmax, sigma=sigma)

    plot_synthetic_data(x, t, w)

    K = 10

    run_cv( K, 7, x, t, testx, testt, randomize_data=False, title='{0}-fold CV'.format(K) )


def fitpoly(x, t, model_order):
    """
    Given "training" data in input feature sequence x and
    corresponding target value sequence t, and a specified
    polynomial of order model_order, determine the linear
    least mean squared (LMS) error best fit for parameters w,
    using the generalized matrix normal equation.
    model_order is a non-negative integer, n, representing the
    highest polynomial order term of the polynomial model:
        t = w0*x^0 + w1*x^1 + w2*x^2 + ... wn*x^n
    :param x: sequence of 1-dimensional input data features
    :param t: sequence of target response values
    :param model_order: integer representing the maximum order of the polynomial model
    :return: parameter vector w
    """

    # Construct the empty design matrix
    # numpy.zeros takes a tuple representing the number of
    # rows and columns, (rows,columns), filled with zeros.
    # The number of columns is model_order+1 because a model_order
    # of 0 requires one column (filled with input x values to the
    # power of 0), model_order=1 requires two columns (first input x
    # values to power of 0, then column of input x values to power 1),
    # and so on...
    X = numpy.zeros((x.shape[0], model_order+1))
    # Fill each column of the design matrix with the corresponding
    for k in range(model_order+1):  # w.size
        X[:, k] = numpy.power(x, k)

    print 'model_order', model_order
    print 'x.shape', x.shape
    print 'X.shape', X.shape
    print 't.shape', t.shape

    #### YOUR CODE HERE ####
    # Calculate w vector (as an numpy.array)
    #w = numpy.dot(numpy.dot(numpy.linalg.inv(numpy.dot(numpy.transpose(X),X)), numpy.transpose(X)), t)
    w = numpy.dot(numpy.dot(numpy.linalg.inv(numpy.dot(numpy.transpose(X),X)), numpy.transpose(X)), t)

    print 'w.shape', w.shape
    print 'wHat: ', w

    return w




# Main Function
if __name__ == '__main__':
    main()
