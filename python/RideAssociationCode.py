
# Accessing CSVs stored locally and Forming a Summary matrix, S
filename = 'yellow_tripdata_2019-02.csv'

#Call for a zone summary
def ZoneSummary(filename):
    #Build S
    absolute_path = os.path.abspath(os.path.dirname(filename))
    df = pd.read_csv('./data/'+filename)
    S = np.zeros((n,n))

    for k in range(0, df['PULocationID'].shape[0]):
        j = df['PULocationID'][k]-1
        i = df['DOLocationID'][k]-1
        S[i, j] += 1

    if S.sum() == df['PULocationID'].shape[0]:
        with open('Summary.pickle', 'wb') as f:
            pickle.dump(S, f)

        return S
    else:
        print("Error Generating Summary Matrix")

#Call for a borough summary
def BoroughSummary(filename):
    # Borough Dictionary
    boroughs = {0: 'EWR',
            1: 'Queens',
            2: 'Bronx',
            3: 'Manhattan',
            4: 'Staten Island',
            5: 'Brooklyn',
            6: 'Unknown'}

    # Reverse Dictionary #
    reverse_boroughs = {v:k for k,v in boroughs.items()}

    #Build S
    absolute_path = os.path.abspath(os.path.dirname(filename))
    df = pd.read_csv('./data/'+filename)
    S = np.zeros((n,n))

    for k in range(0, df['PULocationID'].shape[0]):
        jtemp = df['PULocationID'][k]-1
        j = reverse_boroughs[lookup['Borough'][jtemp]]


    itemp = df['DOLocationID'][k]-1
    i = reverse_boroughs[lookup['Borough'][itemp]]
    S[i, j] += 1

    if S.sum() == df['PULocationID'].shape[0]:
        with open('Summary.pickle', 'wb') as f:
            pickle.dump(S, f)

        return S
    else:
        print("Error Generating Summary Matrix")

# Call on a summary matrix to calculate all coefficients
def Lift(X):
    DO = X.sum(axis = 1)
    PU = X.sum(axis = 0)

    Sum = X.sum()
    Supp = X/Sum

    PU_Supp = PU/Sum
    DO_Supp = DO/Sum


    Lift = copy.copy(X/Sum)
    n = X.shape[0]
    for i in range(0, n):
        for j in range(0,n):
            if DO_Supp[i]*PU_Supp[j] == 0:
                Lift[i, j] = 0
            else:
                Lift[i, j] = Supp[i, j]/(DO_Supp[i]*PU_Supp[j])
    return Lift


# Call to visualize lift values from a summary matrix
def plotlift(L, title = "Title here", fig_dim = (10,10)):
    plt.figure(figsize = fig_dim)
    plt.imshow(np.log(L), cmap = 'Pastel1')
    plt.title(title)


    # plt.xticks(np.arange(n), list(boroughs.values())[:-1])
    # plt.yticks(np.arange(n), list(boroughs.values())[:-1])

    plt.ylabel("Dropoffs")
    plt.xlabel('Pickups')

    plt.colorbar()


    width, height = L.shape
    for x in range(width):
         for y in range(height):
             plt.annotate(str(format(L[x][y], ".2f")), xy = (x,y), horizontalalignment = 'center', verticalalignment = 'center')
       