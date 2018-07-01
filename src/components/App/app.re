[%bs.raw {|require('./app.css')|}];
[@bs.module] external logo : string = "../../assets/logo.svg";

let component = ReasonReact.statelessComponent("App");

let make = (~currentRoute, _children) => {
  ...component,
  render: _self =>
    <div className="App">
      <main>
        <ReactTransitionGroup.TransitionGroup>
          (Config.routeToComponent(currentRoute))
        </ReactTransitionGroup.TransitionGroup>
      </main>
    </div>,
};