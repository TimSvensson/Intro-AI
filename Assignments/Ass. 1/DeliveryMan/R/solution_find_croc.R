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
    
    # belife[,1] - prev belife
    # belife[,2] - crnt belife
    belife = matrix( 0, NUMBER_OF_WATERHOLES, 2 )
    
    if ( length(moveInfo$mem) == 0 )
    {
        belife[,1] = matrix( ( 1 / NUMBER_OF_WATERHOLES ), 1, NUMBER_OF_WATERHOLES )
    }
    else
    {
        belife[,1] = moveInfo$mem
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
    
    belife = updateBelife( belife, observations, transitions )
    
    moveInfo$mem = belife[,2]
    moveInfo$moves = bfs( positions[3], which.max(belife[,2]), edges )
    
    return( moveInfo )
}

updateBelife = function( belife, observations, transitions )
{
    # belife = matrix( p, NUMBER_OF_WATERHOLES, 2 )
    # observations <- R^(NoW x NoW)
    # transitions R^(NoW x NoW)
    
    f = matrix( as.vector( belife[,1] ),
                nrow = NUMBER_OF_WATERHOLES,
                ncol = 2,
                byrow = TRUE )
    
    b = matrix( rep.int(1, 2*NUMBER_OF_WATERHOLES),
                nrow = NUMBER_OF_WATERHOLES,
                ncol = 2 )
    
    f[,2] = forward( belife[,1], observations, transitions )
    b[,1] = backward( observations, transitions )
    belife = smooth( f, b )
    
    return( belife )
}

forward = function( belife, observations, transitions )
{
    return( belife %*% transitions %*% makeDiagonalMatrix( observations ) )
}

backward = function( observations, transitions )
{
    return( matrix(1,1,NUMBER_OF_WATERHOLES) %*% transitions %*% makeDiagonalMatrix( observations ) )
}

smooth = function( f, b )
{
    fb = f * b
    
    
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
            options = getOptions( crnt_position, edges )
            if ( is.na( match( destination, options ) ) )
            {
                # taken from https://stat.ethz.ch/pipermail/r-help/2008-July/167216.html
                # which(abs(x-your.number)==min(abs(x-your.number)))
                
                d = options - destination
                option_i = which( abs( d ) == min( abs( d )))
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

bfs = function( origin, goal, edges )
{
    # visited[i,1] = node
    # visited[i,2] = previouse node
    v = matrix( 0, NUMBER_OF_WATERHOLES, 2 )
    f = matrix( 0, NUMBER_OF_WATERHOLES, 2 )
    
    v.pointer = 1
    f.head = 1
    f.tail = 2
    f[1,] = c( origin, 0 )
    moves = c(0,0)
    
    goal.found = FALSE
    
    while ( goal.found == FALSE )
    {   
        if ( any( v[,1] == goal ) )
        {
            # set moves to return
            origin.found = FALSE
            node = v[which( v[,1] == goal ),]
            
            while ( origin.found == FALSE )
            {
                if ( node[1] == origin )
                {
                    origin.found = TRUE
                }
                else
                {
                    moves[2] = moves[1]
                    moves[1] = node[1]
                    node = v[which( v[,1] == node[2] ),]
                }
            }
            goal.found = TRUE
        }
        else
        {
            # 1. Expand frontier using the "top" of the frontier
            options = getOptions( f[f.head,1], edges )
            for ( i in 1:length( options ) )
            {
                if ( !any( options[i] == v ) &
                     !any( options[i] == f ) )
                {
                    f[f.tail,1] = options[i]
                    f[f.tail,2] = f[f.head]
                    f.tail = f.tail + 1
                }
            }
            # 2. Put top of frontier in visited
            v[v.pointer,] = f[f.head,]
            v.pointer = v.pointer + 1
            f.head = f.head + 1
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

averageTest <- function(tests){
    sum = 0
    for (i in 1:tests) {
        sum=sum+runWheresCroc( makeMoves = Steve_Irwin,
                               pause = 0)
        if(i%%10==0){
            print(i)
            print(sum/i)
        }
    }
    print(sum/i)
    return(0)
}

# 
# Run
# 

# averageTest(500)
runWheresCroc( Steve_Irwin, showCroc=T, pause=1 )