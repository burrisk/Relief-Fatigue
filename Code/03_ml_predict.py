
# coding: utf-8

# In[1]:

import pandas as pd
import numpy as np
import math
import matplotlib.pyplot as plt
from sklearn.cross_validation import KFold
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.metrics import cohen_kappa_score
from scipy.special import expit
from scipy.special import logit
from sklearn.cross_validation import train_test_split
get_ipython().magic('matplotlib inline')


# In[2]:

#Turn off SettingWithCopyWarning()
pd.options.mode.chained_assignment = None


# In[161]:

df = pd.read_csv('../Data/pitch_swing.csv', index_col=0)
inds = np.random.choice(df.shape[0], df.shape[0], replace=False)
df = df.iloc[inds].copy()

df_all = pd.read_csv('../Data/AllStandardPitches.csv', index_col=0)
inds_all = np.random.choice(df_all.shape[0], df_all.shape[0], replace=False)
df_all = df_all.iloc[inds_all].copy()


# In[5]:

list(df.pitch_type.unique())


# # Get best leaf for each pitch_type

# **Also return table of results**

# In[7]:

#k-fold cross validation
def get_best_leaf(df,pitch_list,leaf_list,covariates,folds = 10):

    fits={}
    imps = {}

    for pitch in pitch_list:
        df_p = df[df.pitch_type==pitch]
        fits[pitch] ={}
        Y = df_p.whiff
        X = df_p[covariates]
        
        X = pd.concat([pd.get_dummies(X[col]).ix[:, :-1] 
                       if X[col].dtype == object or hasattr(X[col], 'cat')
                       else X[col]
                       for col in X.columns], axis=1)
        avg_scores = []; avg_kappas = [];
        for leaf in leaf_list:
            scores = []; kappas = []; 
            clf = RandomForestClassifier(min_samples_leaf=leaf)

            for train, test in KFold(Y.shape[0], n_folds=folds):
                #Fit model
                clf.fit(X.iloc[train], Y.iloc[train])

                #Judge the model
                scores.append(clf.score(X.iloc[test], Y.iloc[test]))
                preds = clf.predict(X.iloc[test])
                kappas.append(cohen_kappa_score(preds, Y.iloc[test]))

            #print('\nNumber of Leaves: {}'.format(leaf))
            #print('Avg Score: {0:.3f}'.format(np.mean(scores)))
            #print('Sd Score: {0:.2f}'.format(np.std(scores)))
            #print('Avg Kappa: {0:.3f}'.format(np.mean(kappas)))
            avg_scores.append(round(np.mean(scores),3))
            avg_kappas.append(round(np.mean(kappas),3))

        best_k = np.argmax(avg_kappas)
        best_s = np.argmax(avg_scores)
        print('\nPitch Type: {}'.format(pitch))
        print("Percent Whiffs: ", round(1-Y.mean(),3))
        print('Leaves/Score/Kappa for best:')
        print('Score: ',leaf_list[best_s],"/",avg_scores[best_s],"/",avg_kappas[best_s])
        print('Kappa: ',leaf_list[best_k],"/",avg_scores[best_k],"/",avg_kappas[best_k])

        fits[pitch]['perc_whiff'] = round(1-Y.mean(),3)
        fits[pitch]['score'] = avg_scores[best_s]
        fits[pitch]['num_pitches'] = int(len(Y))
        fits[pitch]['leaf'] = int(leaf_list[best_s])
        fits[pitch]['kappa'] = avg_kappas[best_s]

    return(fits)



# In[162]:

leafs = [1,5,10,25,50];
pitch_types = list(df.pitch_type.unique())
covars = ['start_speed','end_speed','p_throws','pfx_x','pfx_z','vx0','vy0','vz0','ax','ay','az','break_y',
         'break_angle','spin_dir','spin_rate']
best = get_best_leaf(df,pitch_types,leafs,covars)


# In[155]:

best_df = pd.DataFrame(best).T
out_best = best_df[['kappa','perc_whiff','score']]
best_df.to_csv('final_avg_scores.csv')
print(out_best.to_latex())


# # Get Predicted Means Table

# In[54]:

def get_pred_means_table(df,covariates,leaf, subset = None):
    df_p_train, df_p_test = train_test_split(df, test_size=0.5, random_state=42)

    #Test and train X datasets
    Y_train = df_p_train.whiff
    Y_test = df_p_test.whiff
    X_train = df_p_train[covariates]

    X_test = df_p_test[covariates]


    X_train = pd.concat([pd.get_dummies(X_train[col]).ix[:, :-1] 
                   if X_train[col].dtype == object or hasattr(X_train[col], 'cat')
                   else X_train[col]
                   for col in X_train.columns], axis=1)

    X_test = pd.concat([pd.get_dummies(X_test[col]).ix[:, :-1] 
                   if X_test[col].dtype == object or hasattr(X_test[col], 'cat')
                   else X_test[col]
                   for col in X_test.columns], axis=1)

    #Fit the Random Forest with the best leaf
    clf = RandomForestClassifier(min_samples_leaf=leaf)
    clf.fit(X_train,Y_train)

    print(1-Y_test.mean())
    print(clf.score(X_test,Y_test))
    #Predict probabilities
    pred_probs = clf.predict_proba(X_test)[:,1] #predicted probability of strike

    #Take the logit
    pred_stuff = logit(pred_probs)

    pred_mat = df_p_test[["pitcher_name"]].copy()
    pred_mat['pred_stuff'] = pred_stuff
    
    #Get rid of people with predicted probability 0
    pred_mat['pred_stuff'] = pred_mat['pred_stuff'][pred_mat['pred_stuff']>-math.inf]
    
    #mean/sd for standardization
    overall_mean = pred_mat.pred_stuff.mean()
    overall_sd = pred_mat.pred_stuff.std()
    
    #Standardize
    pred_mat['pred_z'] = (pred_mat['pred_stuff'] - overall_mean)/overall_sd
    
    #group by pitcher name
    #average the stuff
    group_pred = pred_mat.groupby('pitcher_name')

    mapped_funs = {'pred_z': {'count','mean'}}
    pred_means = group_pred.agg(mapped_funs)

    #Only show me players with more than 100 of that pitch
    if(subset is not None):
        pred_means = pred_means[pred_means[('pred_z','count')]>subset]
    
    pred_means = pred_means.sort_values(('pred_z','mean'),ascending = False)

    return(pred_means)


# In[140]:

def print_top_pitch(df,pitch,covars,leaf,subset, num_top = 5,):
    top_pitch = get_pred_means_table(df[df.pitch_type==pitch],covars,leaf,subset=subset)
    final_tp = top_pitch.ix[0:num_top,('pred_z','mean')]
    final_tp.to_csv('top_overall_' + pitch+'.csv')
    print(pd.DataFrame(final_tp).to_latex())


# In[127]:

print_top_pitch(df,"FF",covars,best['FF']['leaf'])


# In[214]:

print_top_pitch(df,'SI',covars,subset = 1,leaf=best['FT']['leaf'])


# In[153]:

print_top_pitch(df,'CU',covars,subset = 100,leaf=best['CU']['leaf'],num_top=5)


# # Train Model based on best leaf, attach to other dset

# ** Need to train model on 2012/2016 swings, but attach to 2013-2015 all pitches**

# In[17]:

def add_stuff(train_dset, pred_dset, best_fits, covariates):
    
    #Train all the pitches, save the clfs
    all_pitches = train_dset.pitch_type.unique()
    if(len(all_pitches)!=len(best_fits)):
        print('Error: Num pitch types in fits dictionary not same as num pitch types in dset')
        return
    
    clf_dict = {}
    for i in range(len(all_pitches)):
        pitch = all_pitches[i]
        
        if(pitch not in best_fits.keys()):
            print(pitch+' not in fits dictionary')
            return
        
        train_df = train_dset[train_dset.pitch_type==pitch]
        X_train = train_df[covariates]

        X_train = pd.concat([pd.get_dummies(X_train[col]).ix[:, :-1] 
                       if X_train[col].dtype == object or hasattr(X_train[col], 'cat')
                       else X_train[col]
                       for col in X_train.columns], axis=1)
        
        Y_train = train_df.whiff
        
        leaf = best_fits[pitch]['leaf']
        clf =  RandomForestClassifier(min_samples_leaf=leaf)
        clf.fit(X_train,Y_train)
        
        ##Now, predict on new data
        pred_df = pred_dset[pred_dset.pitch_type==pitch].copy()
        
        X_pred = pred_df[covariates]
        X_pred = pd.concat([pd.get_dummies(X_pred[col]).ix[:, :-1] 
                       if X_pred[col].dtype == object or hasattr(X_pred[col], 'cat')
                       else X_pred[col]
                       for col in X_pred.columns], axis=1)
        
        #Predict probabilities
        pred_probs = clf.predict_proba(X_pred)[:,1] #predicted probability of strike

        #Take the logit
        pred_stuff = logit(pred_probs)
        
        overall_mean = pred_stuff.mean()
        overall_sd = pred_stuff.std()
        
        pred_z = (pred_stuff - overall_mean)/overall_sd
        
        pred_df['z_stuff'] = pred_z
        
        if(i==0):
            out_mat = pred_df.copy()
        else:
            out_mat = out_mat.append(pred_df)
        
    
    if(out_mat.shape[0]!= pred_dset.shape[0]):
        print("You got more than zero problems big fella")
        
    return(out_mat)

    


# In[197]:

final_train = df[(df.year==2016) | (df.year==2012)]
final_pred = df_all[(df_all.year>=2013)&(df_all.year<=2015)]


# In[206]:

jags_dset = add_stuff(final_train, final_pred,best,covars)


# In[207]:

jags_group = jags_dset.groupby('pitcher_name')

mapped_funs = {'z_stuff': {'count','mean'}}
z_stuff = jags_group.agg(mapped_funs)
jags_dset.to_csv('../Data/all_pitches_pred.csv')

