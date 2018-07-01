[%bs.raw {|require('./home.css')|}];

let component = ReasonReact.statelessComponent("Home");

let make = _children => {
  ...component,
  render: _self =>
    <div className="Home">
      (ReasonReact.string("Game"))
    </div>,
};