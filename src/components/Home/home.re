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
        <div className="logo"> (ReasonReact.string("Skeletype")) </div>
        <img src=skully />
        <div className="start-button" onClick=(handle(click))>
          (ReasonReact.string("Start"))
        </div>
      </div>,
  };
};