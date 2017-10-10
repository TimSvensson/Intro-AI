NUMBER_OF_WATERHOLES = 40

# 
# transitions[ node.to, node.from ]
#

Steve_Irwin = function( moveInfo, readings, positions, edges, probs )
{
    # moveInfo  = List      { $moves, $mem }
    # readings  = Vector    { salinity, phosphate, nitrogen }
    # positions = Vector    { Tourist1, Tourist2, Steve Irwin }
    # edges     = Matrix
    # probs     = List      { $slainity, $phsosphate, $nitrogen }
    
    
    transitions = transitions.make( edges )
    
    #
    # prev_belife = Vector { P( croc at node 1 ), P( croc at node 2 ), ... }
    #
    if ( length(moveInfo$mem) == 0 )
    {
        prev_belife = matrix( ( 1 / NUMBER_OF_WATERHOLES ), 1, NUMBER_OF_WATERHOLES )
    }
    else
    {
        prev_belife = moveInfo$mem
    }
    observations = getNormalizedReadings( readings, probs )
    
    if ( !is.na( positions[1] ) )
    {
        if ( positions[1] > 0 )
        {
            observations[positions[1]] = 0
        }
        else
        {
            observations = matrix( 0, 1, NUMBER_OF_WATERHOLES )
            observations[ 1, positions[1] ] = 1
        }
    }
    if ( !is.na( positions[2] ) )
    {
        if ( positions[2] > 0 )
        {
            observations[positions[2]] = 0
        }
        else
        {
            observations = matrix( 0, 1, NUMBER_OF_WATERHOLES )
            observations[ 1, positions[2] ] = 1
        }
    }
    
    crnt_belife = getCurrentBelife( prev_belife, observations, transitions )
    
    print("Most probable waterhole")
    print(which.max(crnt_belife))
    
    moveInfo$mem = crnt_belife
    moveInfo$moves = getMoves( positions[3], which.max(crnt_belife), edges )
    print("moves")
    print(moveInfo$moves)
    
    return( moveInfo )
}

getCurrentBelife = function( previouse_belife, observations, transitions )
{
    
    current_belife = previouse_belife %*% makeDiagonalMatrix( observations ) %*% transitions
    
    return( current_belife )
}

forward = function()
{
    
}

backward = function()
{
    
}

getMoves = function( origin, destination, edges )
{
    print("Origin")
    print(origin)
    print("Destination")
    print(destination)
    
    moves = vector( "numeric", length=2 )
    crnt_position = origin
    
    for ( i in 1:2 )
    {
        if ( crnt_position == destination )
        {
            moves[ i ] = 0
        }
        else
        {
            print("Options")
            options = getOptions( crnt_position, edges )
            print(options)
            
            if ( is.na( match( destination, options ) ) )
            {
                # taken from https://stat.ethz.ch/pipermail/r-help/2008-July/167216.html
                # which(abs(x-your.number)==min(abs(x-your.number)))
                
                d = options - destination
                print("Difference")
                print(d)
                
                option_i = which( abs( d ) == min( abs( d )))
                print("Option to choose")
                print(option_i)
                print(options[ option_i ])
                
                moves[ i ] = options[ option_i ]
                
            }
            else
            {
                moves[ i ] = options[ match( destination, options ) ]
            }
            crnt_position = moves[ i ]
        }
    }
    
    return( moves )
}

makeDiagonalMatrix = function( vector )
{
    matrix = matrix(0, length(vector), length(vector))
    for (i in 1:length(vector))
    {
        matrix[i,i] = vector[i]
    }
    return( matrix )
}

transitions.make = function( edges )
{
    m = matrix(0, NUMBER_OF_WATERHOLES, NUMBER_OF_WATERHOLES)
    
    for ( from in 1:NUMBER_OF_WATERHOLES )
    {
        possible_transitions = getOptions( from, edges )
        for ( to in possible_transitions )
        {
            m[ to, from ] = 1 / length(possible_transitions)
        }
    }
    
    return(m)
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