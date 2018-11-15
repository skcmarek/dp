module Model exposing (EdgeRepres, GraphRepres, Model, Msg(..), NodeRepres, addNodeToGraph, init, makeNode, update)

import Graph
import IntDict
import Port


type alias GraphRepres =
    Graph.Graph NodeRepres EdgeRepres


type alias NodeRepres =
    { id : Graph.NodeId
    , label : String
    , definition : String
    }


type alias EdgeRepres =
    { id : Int
    , from : Graph.NodeId
    , to : Graph.NodeId
    , label : String
    , definition : String
    }


type alias Model =
    { graph : GraphRepres
    , nodeId : Int
    , edgeId : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { graph = Graph.empty
      , nodeId = 0
      , edgeId = 0
      }
    , Cmd.none
    )


makeNode : Int -> String -> String -> NodeRepres
makeNode id label definition =
    { id = id
    , label = label
    , definition = definition
    }


type Msg
    = AddEdge EdgeRepres
    | AddNode NodeRepres


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddNode node ->
            { model | graph = addNodeToGraph node model.graph, nodeId = model.nodeId + 1 }
                ! [ Port.addNode (Port.initNode node.id node.label) ]

        AddEdge edge ->
            ( model, Cmd.none )


addNodeToGraph : NodeRepres -> GraphRepres -> GraphRepres
addNodeToGraph nodeRepres =
    let
        addNode =
            Graph.Node nodeRepres.id nodeRepres
    in
    Graph.insert (Graph.NodeContext addNode IntDict.empty IntDict.empty)
