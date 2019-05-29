#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Dec 19 08:44:50 2018

@author: paula
"""
from pandas import DataFrame
from pandas import concat
from pandas import read_csv
from sklearn.preprocessing import MinMaxScaler
from keras.models import Sequential
from keras.layers import Dense, Dropout
from keras.layers import LSTM
from matplotlib import pyplot
import numpy
from numpy import concatenate
from keras import optimizers



def mean_absolute_percentage_error(y_true, y_pred): 
    y_true, y_pred = numpy.array(y_true), numpy.array(y_pred)
    return numpy.mean(numpy.abs((y_true - y_pred) / y_true)) * 100

def series_to_supervised(dataset,data, n_in=1, n_out=1, dropnan=True):
	n_vars = 1 if type(data) is list else data.shape[1]
	df = DataFrame(data)
	cols, names = list(), list()
	# input sequence (t-n, ... t-1)
	for i in range(n_in, 0, -1):
		cols.append(df.shift(i))
		names += [(str(list(dataset.columns.values)[j])+'var%d(t-%d)' % (j+1, i)) for j in range(n_vars)]
	# forecast sequence (t, t+1, ... t+n)
	for i in range(0, n_out):
		cols.append(df.shift(-i))
		if i == 0:
			names += [(str(list(dataset.columns.values)[j])+'var%d(t)' % (j+1)) for j in range(n_vars)]
		else:
			names += [(str(list(dataset.columns.values)[j])+'var%d(t+%d)' % (j+1, i)) for j in range(n_vars)]
	# put it all together
	agg = concat(cols, axis=1)
	agg.columns = names
	# drop rows with NaN values
	if dropnan:
		agg.dropna(inplace=True)
	return agg

# evaluate the model on a dataset, returns RMSE in transformed units
def evaluate(model, raw_data, scaled_dataset, scaler, offset, batch_size, epoch, nb_epochs):
    # separate
    X= scaled_dataset[:,0:-1]
    # reshape
    reshaped = X.reshape(X.shape[0], 1, X.shape[1])
    # forecast dataset
    # hace el forecast de toda la serie de una sola vez
    yhat = model.predict(reshaped, batch_size=batch_size)   
    yhatX = concatenate((yhat, X[:, 1:]), axis=1)
    inv_yhatX = scaler.inverse_transform(yhatX)
    predictions = inv_yhatX[:,0]
    # invert data transforms on forecast
    # report performance
#    rmse = sqrt(mean_squared_error(raw_data[1:,0], predictions))
    MAPE = mean_absolute_percentage_error(raw_data[1:,0], predictions)
    return MAPE
def evaluate_final(model, raw_data, scaled_dataset, scaler, offset, batch_size, epoch, nb_epochs):
    # separate
    X= scaled_dataset[:,0:-1]
    # reshape
    reshaped = X.reshape(X.shape[0], 1, X.shape[1])
    # forecast dataset
    # hace el forecast de toda la serie de una sola vez
    yhat = model.predict(reshaped, batch_size=batch_size)   
    yhatX = concatenate((yhat, X[:, 1:]), axis=1)
    inv_yhatX = scaler.inverse_transform(yhatX)
    predictions = inv_yhatX[:,0]
    # invert data transforms on forecast
    # report performance
#    rmse = sqrt(mean_squared_error(raw_data[1:,0], predictions))
    #MAPE = mean_absolute_percentage_error(raw_data[1:,0], predictions)
#    print(predictions)
    return predictions

 
# fit an LSTM network to training data
def fit_lstm(train, test, raw, scaler, batch_size, nb_epoch, neurons,activation,optim):
    X, y = train[:, 0:-1], train[:, -1]
    X = X.reshape(X.shape[0], 1, X.shape[1])
    # prepare model
    model = Sequential()
    model.add(LSTM(neurons, activation = activation , batch_input_shape=(batch_size, X.shape[1], X.shape[2]), stateful=True))
    #model.add(Dropout(0.2))
    model.add(Dense(1))
    model.compile(loss='mean_squared_error', optimizer= 'RMSprop')
    # fit model
    train_rmse, test_rmse = list(), list()
    for i in range(nb_epoch):
        model.fit(X, y, epochs=1, batch_size=batch_size, verbose=0, shuffle=False)
        model.reset_states()
        # evaluate model on train data
        raw_train = raw[-(len(train)+len(test)+1):-len(test)]
        train_rmse.append(evaluate(model, raw_train, train, scaler, 0, batch_size , i, nb_epoch))
        model.reset_states()
        # evaluate model on test data
        raw_test = raw[-(len(test)+1):]
        test_rmse.append(evaluate(model, raw_test, test, scaler, 0, batch_size, i, nb_epoch))
        model.reset_states()
    yhat = evaluate_final(model, raw_test, test, scaler, 0, batch_size, i, nb_epoch)
    history = DataFrame()
    history['train'], history['test'] = train_rmse, test_rmse
    return history, yhat

def run(repeats, batches, n_epochs, n_neurons, activation,optim):
    # load dataset
    dataset = read_csv('conjuntofc.csv', header=0, parse_dates=[0], index_col=0, squeeze=True)
    dataset = dataset[-73:]
    dataset = dataset[dataset.columns[::-1]]
    # transform data to be stationary
    # mark all NA values with 0
    dataset['GASOLINA'].fillna(0, inplace=True)
    dataset['Superficie.a.construir.M.m..'].fillna(0, inplace=True)
    raw_values = dataset.values
#    print(dataset.head())
#    print(dataset.tail())

    # specify columns to plot
#    groups = [0,1,2,3,4]
#    i = 1
#    # plot each column
#    pyplot.figure()
#    for group in groups:
#        pyplot.subplot(len(groups), 1, i)
#        pyplot.plot(raw_values[:, group])
#        pyplot.title(dataset.columns[group], y=0.5, loc='right')
#        i += 1
#    #pyplot.show()

    raw_values = raw_values.astype('float32')
    # normalize features
    scaler = MinMaxScaler(feature_range=(0, 1))
    scaled = scaler.fit_transform(raw_values)
    # frame as supervised learning
    reframed = series_to_supervised(dataset,scaled, 1, 1)

    # drop columns we don't want to predict
    reframed.drop(reframed.columns[[6,7,8,9]], axis=1, inplace=True)
    values = reframed.values
    ntest = 12
    train = values[:-ntest, :]
    test = values[-ntest:, :]
    results = DataFrame()
    for e in n_epochs:
        # run diagnostic tests
        result_epoch = list()
        predictions = DataFrame()
        for i in range(repeats):
            history,predictions[str(i)] = fit_lstm(train, test, raw_values, scaler, batches, e, n_neurons,activation,optim)
            pyplot.plot(history['train'], color='blue')
            pyplot.plot(history['test'], color='orange')
            print('%d) TrainMAPE=%f, TestMAPE=%f' % (i, history['train'].iloc[-1], history['test'].iloc[-1]))
            pyplot.title('repeats =' + str(31+i) + 'epochs = '+ str(e) +'_batch ='+str(batches)+'_neurons=' + str(n_neurons)+ 'optimizer = '+optim, fontdict=None, loc='center')
            pyplot.xlabel('epochs')
            pyplot.ylabel('MAPE')
            history.to_csv('repeat='+ str(i)+'optimizador='+ str(optim)+'.csv')
            result_epoch.append(history['test'].iloc[-1])
        predictions.to_csv('predictions.csv')
        pyplot.savefig('epochs='+str(e) +'_batch ='+str(batches)+'_neurons=' + str(n_neurons) + '_activacion='+activation + 'optimizacion ='+optim+'.eps', format='eps', dpi=1000)   
        pyplot.show()
        results[str(e)] = result_epoch
    results.to_csv('BP_epochs_diagnostic_epochs='+str(e)+'_batch ='+str(batches)+'_neurons=' + str(n_neurons) + '__activacion='+activation +'optimizacion ='+optim+'.csv')
    #results.boxplot()
    #pyplot.ylabel('MAPE')
    #pyplot.savefig('definitivo/BP_epochs_diagnostic_epochs='+str(e)+'_batch ='+str(batches)+'_neurons=' + str(n_neurons) + '__activacion='+activation + 'MV.png')
    #pyplot.show()
    results.describe()
    
    
    
def main():
  optimizer = ['rmsprop']
  for o in optimizer:
    neurons = [100]
    for n in neurons:
        batch_size = [2]
        for b in batch_size:
            run(repeats =100 , batches = b, n_epochs= [100] , n_neurons = n, activation = 'relu', optim = o)