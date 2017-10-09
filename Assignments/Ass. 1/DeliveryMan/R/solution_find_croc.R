NUMBER_OF_WATERHOLES = 40

# 
# 
# 
# 
TRANSITIONS = matrix( 0, NUMBER_OF_WATERHOLES, NUMBER_OF_WATERHOLES )

Steve_Irwin = function( moveInfo, readings, positions, edges, probs )
{
    # moveInfo  = List      { $moves, $mem }
    # readings  = Vector    { salinity, phosphate, nitrogen }
    # positions = Vector    { Tourist1, Tourist2, Croc Hunter }
    # edges     = Matrix
    # probes    = List
    
    TRANSITIONS.make(edges)
}


TRANSITIONS.make = function(edges)
{
    for ( y in 1:NUMBER_OF_WATERHOLES )
    {
        print(y)
        possible_transitions = getOptions(y, edges)
        print(possible_transitions)
        
        for ( x in possible_transitions )
        {
            TRANSITIONS[x,y] = 1 / length(possible_transitions)
        }
    }
    print( TRANSITIONS )
    return( TRANSITIONS )
}

getNormalizedReadings = function(readings, probs) {
    # Readings kommer vara tre v??rden
    # F??r alla rader i probs, j??mf??r och lagra v??rdet samt koordinater
    largestPointSalinity = 0
    largestPointPhosphate = 0
    largestPointNitrogen = 0
    largestCombined = 0
    largestPointCombined = 0
    probabilityContainer = vector("numeric", length=40)
    
    for(i in 1:nrow(probs$salinity)) {
        
        salinityLatest = dnorm(readings[[1]], probs$salinity[i,1], probs$salinity[i,2])
        phosphateLatest = dnorm(readings[[2]], probs$phosphate[i,1], probs$phosphate[i,2])
        nitrogenLatest = dnorm(readings[[3]], probs$nitrogen[i,1], probs$nitrogen[i,2])
        
        combinedLatest = salinityLatest*phosphateLatest*nitrogenLatest
        probabilityContainer[i] = combinedLatest
        
        if (combinedLatest > largestCombined) {
            largestCombined = combinedLatest
            largestPointCombined = i
        }
    }
    
    # Take the list of non-normalized probabilities, normalize each value individually and put these in a new list
    probabilityContainerNormalized = vector("numeric", length = 40)
    for(i in 1:length(probabilityContainer)) {
        nextValue = probabilityContainer[i] / Reduce("+", probabilityContainer)
        probabilityContainerNormalized[i] = nextValue
    }
    
    return (probabilityContainerNormalized)
}

# 
# Run
# 

runWheresCroc( Steve_Irwin, showCroc=T, pause=1 )