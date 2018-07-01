type routes =
  | Home
  | Game;

let routeToString =
  fun
  | Home => "/"
  | Game => "/game";

let urlToRoute: ReasonReact.Router.url => routes =
  url =>
    switch (url.path) {
    | ["game"] => Game
    | _ => Home
    };

let routeToTitle = route =>
  switch (route) {
  | Home => "Home"
  | Game => "Game"
  };

let routeToComponent = currentRoute => {
  let withCSSTransition = (component, route) =>
    <ReactTransitionGroup.CSSTransition
      show=true
      timeout=900
      key=(routeToTitle(route))
      classNames="routeTransition">
      component
    </ReactTransitionGroup.CSSTransition>;
  switch (currentRoute) {
  | Home => withCSSTransition(<Home />, Home)
  | Game => withCSSTransition(<Game />, Game)
  };
};