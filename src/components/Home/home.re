[%bs.raw {|require('./home.css')|}];
[@bs.module] external skully : string = "../../assets/skull-moving.gif";

let component = ReasonReact.statelessComponent("Home");
let make = _children => {
  let click = (event, self) => {
    Js.log("EXECUTE");
    ReactEventRe.Mouse.preventDefault(event);
    ReasonReact.Router.push("/game");
  };
  {
    ...component,
    render: ({state, handle, send}) =>
      <div className="Home">
        <img className="logo" src=skully />
        <div className="tagline"> (ReasonReact.string("Skeletype")) </div>
        <div className="eightbit-btn" onClick=(handle(click))>
          (ReasonReact.string("Start"))
        </div>
      </div>,
  };
};