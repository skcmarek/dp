module Graph.DOT
    exposing
        ( Rankdir(..)
        , Styles
        , defaultStyles
        , output
        , outputWithStyles
        , outputWithStylesAndAttributes
        )

{-| This module provides a means of converting the `Graph` data type into a
valid [DOT](https://en.wikipedia.org/wiki/DOT_(graph_description_language))
string for visualizing your graph structure.

You can easily preview your graph by inserting the generated string into an
online GraphViz tool like <https://dreampuf.github.io/GraphvizOnline/>.

You can also dynamically draw your graph in your application by sending the
string over a port to the javascript version of the GraphViz library,
<https://github.com/mdaines/viz.js/> (see the examples there fore more
specifics on how to embed the generated visualization).

@docs output


# Attrs

GraphViz allows for customizing the graph's look via "Attrs."

@docs Styles, Rankdir, defaultStyles, outputWithStyles, outputWithStylesAndAttributes

-}

import Dict exposing (Dict)
import Graph exposing (Edge, Graph, Node, edges, get, nodes)


{-| Converts a `Graph` into a valid DOT string.
Note that you must supply conversion functions for node labels and edge labels
to `Maybe String`s.

When a conversion function returns `Nothing`, no _label_ attribute is output.
For nodes, GraphViz falls back to displaying node ids.
For edges, no label is displayed.

-}
output : (n -> Maybe String) -> (e -> Maybe String) -> Graph n e -> String
output =
    outputWithStyles defaultStyles


{-| A type representing the attrs to apply at the graph, node, and edge
entities (subgraphs and cluster subgraphs are not supported).

Note that `Styles` is made up of strings, which loses type safety, but
allows you to use any GraphViz attrs without having to model them out in
entirety in this module. It is up to you to make sure you provide valid
attr strings. See <http://www.graphviz.org/content/attrs> for available
options.

-}
type alias Styles =
    { rankdir : Rankdir
    , graph : String
    , node : String
    , edge : String
    }


{-| Values to control the direction of the graph
-}
type Rankdir
    = TB
    | LR
    | BT
    | RL


{-| A blank `Styles` record to build from to define your own styles.

    myStyles =
        { defaultStyles
            | node = "shape=box, color=blue, style=\"rounded, filled\""
        }

-}
defaultStyles : Styles
defaultStyles =
    Styles TB "" "" ""


{-| Same as `output`, but allows you to add attrs to the graph.
These attrs will be applied to the entire graph.
-}
outputWithStyles : Styles -> (n -> Maybe String) -> (e -> Maybe String) -> Graph n e -> String
outputWithStyles styles mapNode mapEdge graph =
    let
        labelOnly maybeLabel =
            case maybeLabel of
                Nothing ->
                    Dict.empty

                Just l ->
                    Dict.singleton "label" l
    in
    outputWithStylesAndAttributes styles (labelOnly << mapNode) (labelOnly << mapEdge) graph


{-| Same as `outputWithStyles`, but allows each node and edge to include its
own attrs. Note that you must supply a conversion function for node and edge
labels that return a `Dict String String` of the attribute mappings.

Note that you have to take care of setting the appropriate node and edge labels
yourself.

-}
outputWithStylesAndAttributes :
    Styles
    -> (n -> Dict String String)
    -> (e -> Dict String String)
    -> Graph n e
    -> String
outputWithStylesAndAttributes styles nodeAttrs edgeAttrs graph =
    let
        attrAssocs : Dict String String -> String
        attrAssocs =
            Dict.toList
                >> List.map (\( k, v ) -> k ++ "=" ++ Basics.toString v)
                >> String.join ", "

        makeAttrs : Dict String String -> String
        makeAttrs d =
            if Dict.isEmpty d then
                ""
            else
                " [" ++ attrAssocs d ++ "]"

        edges =
            let
                compareEdge a b =
                    case compare a.from b.from of
                        LT ->
                            LT

                        GT ->
                            GT

                        EQ ->
                            compare a.to b.to
            in
            Graph.edges graph
                |> List.sortWith compareEdge

        nodes =
            Graph.nodes graph

        edgesString =
            List.map edge edges
                |> String.join "\n"

        edge e =
            "  "
                ++ Basics.toString e.from
                ++ " -> "
                ++ Basics.toString e.to
                ++ makeAttrs (edgeAttrs e.label)

        nodesString =
            List.map node nodes
                |> String.join "\n"

        node n =
            "  "
                ++ Basics.toString n.id
                ++ makeAttrs (nodeAttrs n.label)
    in
    String.join "\n"
        [ "digraph G {"
        , "  rankdir=" ++ toString styles.rankdir
        , "  graph [" ++ styles.graph ++ "]"
        , "  node [" ++ styles.node ++ "]"
        , "  edge [" ++ styles.edge ++ "]"
        , ""
        , edgesString
        , ""
        , nodesString
        , "}"
        ]
