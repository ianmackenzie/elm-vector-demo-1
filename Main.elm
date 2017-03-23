module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import OpenSolid.Svg as Svg
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.SketchPlane3d as SketchPlane3d
import Time exposing (Time)
import AnimationFrame
import Color exposing (Color)


type alias Face =
    { points : ( Point3d, Point3d, Point3d, Point3d )
    , color : String
    , minDistanceFromPlane : Float
    }


colorString : Color -> String
colorString color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")"


heightFunction : Time -> Float -> Float -> Float
heightFunction time x y =
    let
        r =
            sqrt (x ^ 2 + y ^ 2)

        z =
            r * pi / 15 * sin (pi / 80 * r + time / 100)
    in
        z


grid : Time -> Plane3d -> List Face
grid time plane =
    let
        gridElement ( x, y ) =
            let
                color =
                    Color.hsl (heightFunction time x y / 10) 0.7 0.4
                        |> colorString

                point x y =
                    Point3d ( x, y, heightFunction time x y )

                points =
                    ( point (x - 10) (y - 10)
                    , point x (y - 10)
                    , point x y
                    , point (x - 10) y
                    )
            in
                Face
                    points
                    color
                    (minDistanceFrom plane points)

        range =
            (List.range -12 12) |> List.map (toFloat >> (*) 10)

        matrix =
            range
                |> List.concatMap
                    (\x ->
                        range |> List.map (\y -> ( x, y ))
                    )
    in
        matrix
            |> List.map gridElement


sketchPlane : Time -> SketchPlane3d
sketchPlane time =
    SketchPlane3d.xy
        |> SketchPlane3d.rotateAroundOwn SketchPlane3d.xAxis (degrees -0.025 * time)
        |> SketchPlane3d.rotateAroundOwn SketchPlane3d.yAxis (degrees 0.05 * time)


draw face2d =
    let
        ( p1, p2, p3, p4 ) =
            face2d.points
    in
        Svg.polygon2d
            [ Attributes.fill face2d.color ]
            (Polygon2d [ p1, p2, p3, p4 ])


optimizedProjectInto : SketchPlane3d -> Point3d -> Point2d
optimizedProjectInto sketchPlane point =
    let
        (SketchPlane3d { originPoint, xDirection, yDirection }) =
            sketchPlane

        (Point3d ( x0, y0, z0 )) =
            originPoint

        (Direction3d ( ux, uy, uz )) =
            xDirection

        (Direction3d ( vx, vy, vz )) =
            yDirection

        (Point3d ( x, y, z )) =
            point

        dx =
            x - x0

        dy =
            y - y0

        dz =
            z - z0
    in
        Point2d
            ( dx * ux + dy * uy + dz * uz
            , dx * vx + dy * vy + dz * uz
            )


svgProjection : Time -> List (Svg Msg)
svgProjection time =
    let
        plane =
            sketchPlane time

        project =
            optimizedProjectInto plane
    in
        grid time (SketchPlane3d.plane plane)
            |> List.sortBy .minDistanceFromPlane
            |> List.map
                (\face ->
                    { points =
                        let
                            ( p1, p2, p3, p4 ) =
                                face.points
                        in
                            ( project p1, project p2, project p3, project p4 )
                    , color = face.color
                    }
                        |> draw
                )


optimizedSignedDistanceFrom : Plane3d -> Point3d -> Float
optimizedSignedDistanceFrom plane point =
    let
        (Plane3d { originPoint, normalDirection }) =
            plane

        (Point3d ( x0, y0, z0 )) =
            originPoint

        (Direction3d ( nx, ny, nz )) =
            normalDirection

        (Point3d ( x, y, z )) =
            point
    in
        (x - x0) * nx + (y - y0) * ny + (z - z0) * nz


minDistanceFrom : Plane3d -> ( Point3d, Point3d, Point3d, Point3d ) -> Float
minDistanceFrom plane ( p1, p2, p3, p4 ) =
    let
        d1 =
            optimizedSignedDistanceFrom plane p1

        d2 =
            optimizedSignedDistanceFrom plane p2

        d3 =
            optimizedSignedDistanceFrom plane p3

        d4 =
            optimizedSignedDistanceFrom plane p4
    in
        min (min d1 d2) (min d3 d4)



--List.sortBy (minDistanceFrom plane)


container : ( Float, Float ) -> ( Float, Float ) -> List (Svg Msg) -> Html Msg
container ( minX, minY ) ( maxX, maxY ) svgs =
    let
        width =
            maxX - minX

        height =
            maxY - minY

        topLeftFrame =
            Frame2d
                { originPoint = Point2d ( minX, maxY )
                , xDirection = Direction2d.x
                , yDirection = Direction2d.flip Direction2d.y
                }
    in
        Html.div []
            [ Svg.svg
                [ Attributes.width (toString width)
                , Attributes.height (toString height)
                , Attributes.stroke "white"
                , Attributes.strokeWidth "0.5"
                , Attributes.strokeOpacity "0.5"
                , Attributes.fillOpacity "0.5"
                ]
                (svgs
                    |> List.map
                        (\svg ->
                            (Svg.relativeTo topLeftFrame svg)
                        )
                )
            ]


view : Model -> Html Msg
view model =
    let
        styles =
            [ ( "backgroundColor", "#000000" )
            , ( "height", "-1%" )
            , ( "display", "flex" )
            , ( "justify-content", "center" )
            , ( "flex-wrap", "wrap" )
            ]

        svgs =
            [ 0.25, -0.5, -0.25 ]
                |> List.map
                    (\speed ->
                        container ( -200, -200 ) ( 200, 200 ) (svgProjection (speed * model))
                    )
    in
        Html.div [ style styles ] svgs


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    Time


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )



-- UPDATE


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( newTime, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times Tick
