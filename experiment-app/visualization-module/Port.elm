port module Port exposing (Edge, Node, addEdge, addNode, initEdge, initNode)


type alias Node =
    { id : Int
    , label : String
    }


type alias Edge =
    { id : Int
    , from : Int
    , to : Int
    , label : String
    }


initNode : Int -> String -> Node
initNode id label =
    { id = id
    , label = label
    }


initEdge : Int -> Int -> Int -> String -> Edge
initEdge id from to label =
    { id = id
    , from = from
    , to = to
    , label = label
    }


port addNode : Node -> Cmd msg


port addEdge : Edge -> Cmd msg
