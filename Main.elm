module Main exposing (ContactInfo, Dish, Hamburger(..), HamburgerItems(..), Menu, Model, Msg(..), breakfast_dishes, convertDisplayType, dinner_dishes, dish, dishDescription, dishPhoto, displayDishes, displayFooter, displayInfo, hamburgItem, info, initialModel, main, menuItems, menuName, menus, update, view, viewHamburgerItems)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Markdown exposing (toHtml)



-- MSG


type Msg
    = ToggleHamburgerItems Hamburger
    | HamburgerItemInfo HamburgerItems
    | ActiveMenu Menu
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
    , active_menu : Maybe Menu
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



-- intialModel : () -> ( Model, Cmd Msg )


initialModel =
    ( { background_photo = "photos/3-23-2018.JPG"
      , header_img = "photos/garleek.png"
      , footer_color = "#F4FFFC"
      , sidebar_color = "#3F3E40"
      , contact_info = info
      , hamburger = Closed
      , about = """
Chef Tsering Phuntsok has experience in both western and eastern cuisine. Before professionally studying the art of cooking, young Tsering often went to his Grandmother’s place to watch her cook. His passion began by seeing her love of cooking and, when he was old enough, his Grandma taught him her secrets’.


Later in his youth, Tsering joined Thrangu Rinpoche’s monastery. There, he mastered traditional Tibetan cuisine. While in the monastery, Tsering traveled to both Nepal and India, quickly mastering Nepalese and Indian flavors. Then in the early 2000’s, Tsering decided to leave the monastery and move to the United States where he continued to learn his culinary skills. Furthermore, while dating his now wife, Tsering cooked regularly with her first generation Italian family to master Italian pasta and sauces.


Tsering made his final move to Toronto in 2011 where he continued fine tuning his culinary skills and reinventing the flavors from his past. He decided to open Garleek to share his passion for food from around the world here in Toronto. You will find a unique, yet familiar, flavor in whatever dish you try at Garleek Kitchen.
    """
      , hamburger_items = HamburgerClosed
      , menus = menus
      , active_menu = Nothing
      }
    , Cmd.none
    )


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
    [ Dish "Veggie Momo" Nothing (Just "photos/1.webp")
    , Dish "Beef Momo" Nothing (Just "photos/2.webp")
    , Dish "Beef or Chicken Curry" Nothing (Just "photos/3.webp")
    , Dish "Chowmein" Nothing (Just "photos/4.webp")
    , Dish "Aloo Dum" Nothing (Just "photos/5.webp")
    , Dish "Chana" Nothing Nothing
    , Dish "Chili (pork, chicken or beef)" Nothing Nothing
    , Dish "Thukpa" Nothing (Just "photos/6.webp")
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleHamburgerItems ham ->
            case ham of
                Open ->
                    ( { model | hamburger = Closed }, Cmd.none )

                Closed ->
                    ( { model | hamburger = Open }, Cmd.none )

        HamburgerItemInfo items ->
            case items of
                About ->
                    ( { model | hamburger_items = About }, Cmd.none )

                Contact ->
                    ( { model | hamburger_items = Contact }, Cmd.none )

                Menus ->
                    ( { model | hamburger_items = Menus }, Cmd.none )

                HamburgerClosed ->
                    ( { model | hamburger_items = HamburgerClosed }, Cmd.none )

        ActiveMenu active ->
            ( { model | active_menu = Just active }, Cmd.none )

        None ->
            ( model, Cmd.none )


menuItems : Menu -> Menu
menuItems menu =
    Menu menu.name menu.dishes



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ header [] [ img [ class "header-img", src model.header_img ] [] ]
        , i [ class "hamburger fa fa-bars fa-3x", onClick (ToggleHamburgerItems model.hamburger) ] []
        , viewHamburgerItems model.hamburger
        , displayInfo model
        , displayDishes model.active_menu
        , img [ class "img", src model.background_photo ] []
        , displayFooter model
        ]


viewHamburgerItems : Hamburger -> Html Msg
viewHamburgerItems items =
    case items of
        Open ->
            div [ class "hamburger-items" ] (List.map hamburgItem [ "About", "Contact", "Menus" ])

        Closed ->
            div [] []


hamburgItem : String -> Html Msg
hamburgItem s =
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
            div [ class "contact" ]
                [ h1 [] [ text "Welcome!" ]
                , div [] [ text "Please call us to place your pick-up order" ]
                , div [] [ text model.contact_info.phone ]
                ]

        Menus ->
            div [ class "menus" ] (List.map menuName model.menus)

        HamburgerClosed ->
            div [] []


menuName : Menu -> Html Msg
menuName menu =
    a [ href "#", class "menu", onClick (ActiveMenu menu) ] [ text menu.name ]


displayDishes : Maybe Menu -> Html Msg
displayDishes menu =
    case menu of
        Just aMenu ->
            let
                dishes =
                    aMenu.dishes
            in
            case dishes of
                Just someDishes ->
                    div [ class "dishes" ] (List.map dish someDishes)

                Nothing ->
                    div [] []

        Nothing ->
            div [] []


dish : Dish -> Html Msg
dish d =
    let
        name =
            d.name

        description =
            dishDescription d.description

        photo =
            dishPhoto d.photo
    in
    div [ class "dish" ]
        [ div [ class "dish-name" ] [ text name ]
        , div [ class "dish-description" ] [ text description ]
        , img [ class "dish-photo", src photo ] []
        ]


dishDescription : Maybe String -> String
dishDescription description =
    case description of
        Nothing ->
            ""

        Just desc ->
            desc


dishPhoto : Maybe String -> String
dishPhoto photo =
    case photo of
        Nothing ->
            ""

        Just p ->
            p


displayFooter : Model -> Html Msg
displayFooter model =
    footer [ class "footer", style "background-color" model.footer_color ]
        [ div [] [ text model.contact_info.address ]
        , div [] [ text (model.contact_info.city ++ " " ++ model.contact_info.state ++ " " ++ model.contact_info.zip) ]
        , div [] [ text ("Open everyday" ++ " " ++ model.contact_info.hours) ]
        , a [ href model.contact_info.facebook ] [ i [ class "social-media-icon fa fa-facebook fa-2x" ] [] ]
        , a [ href model.contact_info.instagram ] [ i [ class "social-media-icon fa fa-instagram fa-2x" ] [] ]
        ]


main =
    Browser.element
        { init = \() -> initialModel
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
