module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Markdown exposing (toHtml)

-- MSG

type Msg
    = ToggleHamburgerItems Hamburger
    | HamburgerItemInfo HamburgerItems
    | ActiveMenu String
    | None

type HamburgerItems
    = About
    | Contact
    | Menus
    | HamburgerClosed

type Hamburger
    = Open
    | Closed

-- MODEL

type alias Model = 
    { background_photo : String
    , header_img : String
    , sidebar_color : String
    , footer_color : String
    , contact_info : ContactInfo
    , hamburger : Hamburger
    , about : String
    , hamburger_items : HamburgerItems
    , menus : List Menu
    , menu_dishes : Maybe Menu
    }

type alias Menu =
    { name : String
    , dishes : Maybe (List Dish)
    }

type alias Dish =
    { name : String
    , description : Maybe String
    , photo : Maybe String
    }

type alias ContactInfo =
    { address : String
    , city : String
    , state : String
    , zip : String
    , hours : String
    , phone : String
    , instagram : String
    , facebook : String
    }

initialModel  : Model
initialModel = 
    { background_photo = "photos/3-23-2018.JPG" 
    , header_img = "photos/garleek.png"
    , footer_color = "#F4FFFC"
    , sidebar_color = "#3F3E40"
    , contact_info = info
    , hamburger = Closed
    , about = """
Chef Tsering Phuntsok has experience in both western and eastern cuisine. Before professionally studying the art of cooking, young Tsering often went to his Grandmother’s place to watch her cook. His passion began by seeing her love of cooking and, when he was old enough, his Grandma taught him her secrets’.\n

Later in his youth, Tsering joined Thrangu Rinpoche’s monastery. There, he mastered traditional Tibetan cuisine. While in the monastery, Tsering traveled to both Nepal and India, quickly mastering Nepalese and Indian flavors. Then in the early 2000’s, Tsering decided to leave the monastery and move to the United States where he continued to learn his culinary skills. Furthermore, while dating his now wife, Tsering cooked regularly with her first generation Italian family to master Italian pasta and sauces.\n

Tsering made his final move to Toronto in 2011 where he continued fine tuning his culinary skills and reinventing the flavors from his past. He decided to open Garleek to share his passion for food from around the world here in Toronto. You will find a unique, yet familiar, flavor in whatever dish you try at Garleek Kitchen.
    """
    , hamburger_items = HamburgerClosed
    , menu_dishes = Nothing
    , menus = menus
    }

info = 
    { address = "1500 Queen Street W."
    , city = "Toronto"
    , state = "ON"
    , zip = "M6R 14A"
    , hours = "8am to 9pm"
    , phone = "416-551-0929"
    , instagram = "https://www.instagram.com/garleekkitchen"
    , facebook = "https://www.facebook.com/1142557119180556"
    }

menus = 
    [ Menu "Breakfast" (Just breakfast_dishes)
    , Menu "Lunch/Dinner" (Just dinner_dishes)
    ]
    
breakfast_dishes =
    [ Dish "Potatoes" (Just "Choose either Aloo Dum (potato with gravy) or Aloo Soya (potato without gravy) - comes with your style egg, pan fried or depp fried puris (bread), and Tibetan or sweet tea.") Nothing
    ]

dinner_dishes =
    [ Dish "Veggie Momo" Nothing (Just "photo/1.webp")
    , Dish "Beef Momo" Nothing (Just "photo/2.webp")
    , Dish "Beef or Chicken Curry" Nothing (Just "photo/3.webp")
    , Dish "Chowmein" Nothing (Just "photo/4.webp")
    , Dish "Aloo Dum" Nothing (Just "photo/5.webp")
    , Dish "Chana" Nothing Nothing
    , Dish "Chili (pork, chicken or beef)" Nothing Nothing
    , Dish "Thukpa" Nothing (Just "photo/6.webp")
    ]

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleHamburgerItems msg ->
            case msg of
                Open ->
                    ( { model | hamburger = Closed }, Cmd.none )

                Closed ->
                    ( { model | hamburger =  Open }, Cmd.none )

        HamburgerItemInfo msg ->
            case msg of
                About ->
                    ( { model | hamburger_items = About }, Cmd.none )

                Contact ->
                    ( { model | hamburger_items = Contact }, Cmd.none )

                Menus ->
                    ( { model | hamburger_items = Menus }, Cmd.none )

                HamburgerClosed ->
                    ( { model | hamburger_items = HamburgerClosed }, Cmd.none )

        ActiveMenu msg ->
            let
               menu = List.filter (\menu -> String.contains msg menu.name) model.menus
               menu_items = List.head (List.map menuItems menu)
            in
                case menu_items of
                    Just menu_items ->
                        ( { model | menu_dishes = Just menu_items }, Cmd.none )

                    Nothing ->
                        ( { model | menu_dishes = Nothing }, Cmd.none )

        None ->
            ( model , Cmd.none )

menuItems : Menu -> Menu
menuItems menu =
    Menu menu.name menu.dishes

-- VIEW

view : Model -> Html Msg
view model =
    div [ class "wrapper" ] [ header [] [ img [ class "header-img", src model.header_img ] [] ] 
    , i [ class "hamburger fa fa-bars fa-3x", onClick (ToggleHamburgerItems model.hamburger) ] []
    , viewHamburgerItems model
    , displayInfo model
    , displayDishes model.menu_dishes
    , img [ class "img", src model.background_photo] []
    , displayFooter model
    ]

viewHamburgerItems : Model -> Html Msg
viewHamburgerItems model =
    case model.hamburger of
        Open ->
            div [ class "hamburger-items" ] (List.map item ["About", "Contact", "Menu"])
        Closed ->
            div [] []

item : String -> Html Msg
item s =
    a [ href "#", class "hamburger-item", onClick (HamburgerItemInfo (convertDisplayType s)) ] [ text s ]

convertDisplayType : String -> HamburgerItems
convertDisplayType string =
    case string of
        "About" ->
            About

        "Contact" ->
            Contact

        "Menus" ->
            Menus

        _ ->
            HamburgerClosed

displayInfo : Model -> Html Msg
displayInfo model =
    case model.hamburger_items of
        About ->
            Markdown.toHtml [ class "about" ] model.about

        Contact ->
            div [ class "contact" ] [ h1 [] [ text "Welcome!" ]
            , div [] [ text "Please call us to place your pick-up order" ]
            , div [] [ text model.contact_info.phone ]
            ]

        Menus ->
            let
                names = List.map .name model.menus
            in
                div [ class "menus" ] (List.map menuName names)

        HamburgerClosed ->
            div [] []

menuName : String -> Html Msg
menuName name =
    a [ href "#", class "menu", onClick (ActiveMenu name) ] [ text name ] 

displayDishes : Maybe Menu -> Html Msg
displayDishes menu =
    case menu of
        Just menu ->
            let
                dishes = menu.dishes
            in
                case dishes of
                    Just dishes ->
                        div [ class "dishes" ] (List.map dish dishes)

                    Nothing ->
                        div [] []

        Nothing ->
                div [] []

dish : Dish -> Html Msg
dish d =
    let
        name = d.name
        description = dishDescription d.description
        photo = dishPhoto d.photo
    in

        div [ class "dish" ] [ div [] [ text name ]
        , div [] [ text description ]
        ]

dishDescription : Maybe String -> String
dishDescription description =
    case description of
        Nothing ->
            ""

        Just description ->
            description

dishPhoto : Maybe String -> String
dishPhoto photo =
    case photo of
        Nothing ->
            ""

        Just photo ->
            photo

displayFooter : Model -> Html Msg
displayFooter model =
    footer [ class "footer", style [("background-color", model.footer_color)]] [ div [] [ text model.contact_info.address]
        , div [] [ text (model.contact_info.city ++ " " ++ model.contact_info.state ++ " " ++ model.contact_info.zip) ]
        , div [] [ text ("Open everyday" ++ " " ++ model.contact_info.hours) ]
        , a [ href model.contact_info.facebook ] [ i [ class "social-media-icon fa fa-facebook fa-2x" ] []]
        , a [ href model.contact_info.instagram ] [ i [ class "social-media-icon fa fa-instagram fa-2x" ] []]]

init : (Model, Cmd Msg)
init =
    ( initialModel, Cmd.none )

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }

