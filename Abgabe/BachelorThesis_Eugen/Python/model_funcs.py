# from src.utils import common, list_dict_data_tool
# from utils import common, list_dict_data_tool
from scipy.stats import multinomial, dirichlet
import numpy as np
import pandas as pd
from scipy.linalg import block_diag


class MultinomialExpectationMaximizer:
    def __init__(self, K, rtol=1e-4, max_iter=100):
        self._K = K
        self._rtol = rtol
        self._max_iter = max_iter

    def log_lh(self, Y, pi, theta):
        """
        Computes log likelihood of the data given the model
        :param Y: data
        :param pi: pi parameter of multinomial
        :param theta: theta parameter of multinomial
        :return: log likelihood
        """
        mn_probs = np.zeros(Y.shape[0])
        for k in range(Y.shape[1]):
            mn_probs_k = pi[k] * self._multinomial_prob(Y, theta[k])
            mn_probs += mn_probs_k
        mn_probs[mn_probs == 0] = np.finfo(float).eps
        return np.log(mn_probs).sum()

    def _multinomial_prob(self, counts, theta):
        """
        Computes multinomial probability
        :param counts: vector of counts of dimension (C)
        :param theta: theta vector of multinomial parameters of dimension (C)
        :return: probability of the observation given the respective theta
        """
        epsilon = np.finfo(float).eps
        theta = np.maximum(theta, epsilon)
        
        # Normalize theta to ensure it sums to 1
        theta /= theta.sum()
        
        # Calculate the sum of counts
        n = counts.sum(axis=-1)
        
        # Create multinomial distribution and calculate PMF
        m = multinomial(n, theta)
        return m.pmf(counts)


        # n = counts.sum(axis=-1)
        # m = multinomial(n, theta)
        # return m.pmf(counts)

    def _e_step(self, Y, pi, theta):
        """
        Performs E-step, i.e., computes posterior probability as ((prior * likelihood)/evidence)
        :param Y: data points of dimension (N x C)
        :param pi: pi parameter, i.e., mixture weights of dimension (K)
        :param theta: theta parameter, i.e., multinomial probabilities of dimension (K x C)
        :return: tau: probabilities of classes for data points of dimension (N x K)
        """

        N = Y.shape[0]
        K = pi.shape[0]
        weighted_multi_prob = np.zeros((N, K))
        for k in range(K):
            weighted_multi_prob[:, k] = pi[k] * self._multinomial_prob(Y, theta[k])

        denum = weighted_multi_prob.sum(axis=1)
        tau = weighted_multi_prob / denum.reshape(-1, 1)

        # make sure, that tau contains no NaN values and no values above 1 or below 0
        tau = np.nan_to_num(tau, nan=0)
        tau = np.clip(tau, 0, 1)
        tau = tau / tau.sum(axis=1, keepdims=True)

        # Stochastic E-step: Draw Z from multinomial distribution
        Z = np.argmax(np.array([multinomial.rvs(1, tau_i) for tau_i in tau]), axis=1) + 1

        return tau, Z

        # # Compute tau
        # N = Y.shape[0]
        # K = pi.shape[0]
        # weighted_multi_prob = np.zeros((N, K))
        # for k in range(K):
        #     weighted_multi_prob[:, k] = pi[k] * self._multinomial_prob(Y, theta[k])

        # denum = weighted_multi_prob.sum(axis=1)
        # tau = weighted_multi_prob / denum.reshape(-1, 1)

        # return tau

    def _m_step(self, Y, tau, Z):
        """
        Performs M-step, recomputes parameters given posterior classes,
        i.e., new pi is relative frequency of posterior class (tau) of all votes (Y)
        :param Y: data points of dimension (N x C)
        :param tau: probabilities of classes for data points of dimension (N x K)
        :param Z: random variable Z drawn from multinomial distribution for each observation (N x 1)
        :return: pi, theta. pi: mixture weights of dimension (K), theta: multinomial probabilities of dimension (K x C)
        """

        K = Y.shape[1]

        # estimate pi using Z. pi_k = mean(Z == k)
        pi = np.array([np.mean(Z == k) for k in range(1, K + 1)])

        # estimate theta using Z and Y
        # Initialize the theta matrix
        theta = np.zeros((K, K))

        for l in range(1, 11):
            mask = (Z == l)
            numerator = np.sum(Y[mask], axis=0)
            denominator = np.sum(Y[mask])
            theta[l-1, :] = numerator / denominator

        theta = np.nan_to_num(theta, nan=0, posinf=0, neginf=0)
        
        return pi, theta


        # # Compute pi
        # pi = tau.sum(axis=0) / tau.sum()

        # # Compute theta
        # weighted_counts = tau.T.dot(Y)
        # theta = weighted_counts / weighted_counts.sum(axis=-1).reshape(-1, 1)

        # return pi, theta

    # def _compute_loss(self, Y, pi, theta, tau):
    #     """
    #     Computes loss of the model given the data and parameters. Each input is a numpy array.
    #     :param Y: data points of dimension (N x C)
    #     :param pi: pi parameter, i.e., mixture weights of dimension (K)
    #     :param theta: theta parameter, i.e., multinomial probabilities of dimension (K x C)
    #     :param tau: tau parameter, i.e., probabilities of classes for data points of dimension (N x K)
    #     :return:
    #     """

    #     loss = 0
    #     for k in range(pi.shape[0]):
    #         weights = tau[:, k]
    #         loss += np.sum(weights * (np.log(pi[k]) + np.log(self._multinomial_prob(Y, theta[k]))))
    #         loss -= np.sum(np.nan_to_num(weights * np.log(weights), 0))
    #     return loss

    # initialize pi as uniform distribution, theta (= confusion values) as dirichlet since its the conjugate prior to
    # a multinomial
    def _init_params(self, C):
        """
        Initializes parameters for EM algorithm. pi is initialized as equal probabilities, i.e., 1/K ,
        theta (= confusion values) is sampled K (number of latent classes) times from a dirichlet since it's the
        conjugate prior to a multinomial.
        Parameter alpha has C entries, where each value is 2·C. C is the number of observed classes in the data.
        :param C: number of observed classes in data
        :return: pi, theta
        """
        pi = np.array([1 / self._K] * self._K)
        theta = dirichlet.rvs([2 * C] * C, self._K)
        return pi, theta

    def _train_once(self, Y):
        """
        Trains the model once. Initializes parameters, then performs E-step and M-step until convergence.
        :param Y: data points of dimension (N x C)
        :return: pi, theta, tau, loss
        """
        pi, theta = self._init_params(Y.shape[1])
        theta_list = []
        pi_list = []
        tau_list = []
        Z_list = []

        for it in range(self._max_iter):
            tau, Z = self._e_step(Y, pi, theta)
            pi, theta = self._m_step(Y, tau, Z)
            pi_list.append(pi.copy())
            tau_list.append(tau.copy())
            theta_list.append(theta.copy())
            Z_list.append(Z.copy())

        # take the mean of all values after a burn in period
        burn_in = self._max_iter//10
        pi_est = np.mean(pi_list[burn_in:], axis=0)
        tau_est = np.mean(tau_list[burn_in:], axis=0)
        theta_est = np.mean(theta_list[burn_in:], axis=0)

        return pi_est, theta_est, tau_est, theta_list, pi_list, tau_list, Z_list
    
    def _theta_old(self, theta):
        return theta[:, :-1].flatten()

    def _compute_outer_variance(self, vec, mean_vec):
        diff = vec - mean_vec
        return np.outer(diff, diff)

    def _compute_inner_variance(self, theta, Z):
        return block_diag(*[(np.diag(theta[:, :-1][i-1]) - np.outer(theta[:, :-1][i-1], theta[:, :-1][i-1])) / np.sum(Z == i) for i in range(1, 11)])

    def _compute_variance(self, theta_list, Z_list):    
        theta_old_list = list(map(self._theta_old, theta_list[self._max_iter//10:]))
        theta_old_final = np.mean(theta_old_list, axis=0)
        outer_variance = [self._compute_outer_variance(vec, theta_old_final) for vec in theta_old_list]
        Outer_variance = np.sum(outer_variance, axis=0) / (len(outer_variance) - 1)

        inner_variance_list = list(map(self._compute_inner_variance, theta_list[self._max_iter//10:], Z_list[self._max_iter//10:]))
        inner_variance_final = np.mean(inner_variance_list, axis=0)

        return inner_variance_final + Outer_variance

def run_em(Y, K=10, max_iter=1000):
    """
    Runs EM algorithm on data Y with K latent classes. Returns the best model.
    :param Y: data points of dimension (N x C)
    :param K: number of latent classes
    :return: likelihoods, best_pi, best_theta, best_tau
    """
    likelihoods = []
    best_pi = None
    best_theta = None
    best_tau = None
    best_theta_list = None
    best_pi_list = None
    best_tau_list = None
    best_Z_list = None
    variance = None
    plausible = False
    counter = 0

    model = MultinomialExpectationMaximizer(K, max_iter=max_iter)
    while(plausible == False and counter < 10):
        pi, theta, tau, theta_list, pi_list, tau_list, Z_list  = model._train_once(Y)
        plausible = np.sum(theta >= 0.7) == 10
        counter += 1
    vec_theta = model._theta_old(theta)
    variance = model._compute_variance(theta_list, Z_list)
    log_likelihood = model.log_lh(Y, pi, theta)
    likelihoods.append(log_likelihood)
    best_pi = pi
    best_theta = theta
    best_tau = tau
    best_theta_list = theta_list
    best_pi_list = pi_list
    best_tau_list = tau_list
    best_Z_list = Z_list

    return likelihoods, best_pi, best_theta, best_tau, best_theta_list, best_pi_list, best_tau_list, best_Z_list, vec_theta, variance

def sort_matrix(array):
    """
    Sorts an array by the first column.
    :param array: array to sort
    :return: sorted array, matched index
    """
    array = np.copy(array)  # Create a copy of the array to avoid modifying the original

    for col in range(array.shape[1]):
        max_ind = np.argmax(array[:, col])
        # Swap the rows
        array[[col, max_ind], :] = array[[max_ind, col], :]

    return array

def create_Y(data):
    # Converting chosen_category to categorical type
    data['chosen_category'] = data['chosen_category'].astype('category')

    # Grouping by image_filename and chosen_category, and counting the occurrences
    grouped = data.groupby(['image_filename', 'chosen_category'], observed=False, dropna=False).size().reset_index(name='n')

    # Pivoting the DataFrame to have chosen_category as columns
    pivoted = grouped.pivot(index='image_filename', columns='chosen_category', values='n').fillna(0)

    # Adding a new column 'J' which is the row sum excluding the image_filename
    pivoted['J'] = pivoted.sum(axis=1)

    # Resetting the index to get 'image_filename' back as a column
    pivoted = pivoted.reset_index()

    return pivoted
