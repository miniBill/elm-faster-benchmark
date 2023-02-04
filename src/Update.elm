module Update exposing (andThen, map, update)


update : model -> ( model, Cmd msg )
update model =
    ( model, Cmd.none )


map : (model -> model) -> ( model, Cmd msg ) -> ( model, Cmd msg )
map f ( model, cmd ) =
    ( f model, cmd )


andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen f ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            f model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )
