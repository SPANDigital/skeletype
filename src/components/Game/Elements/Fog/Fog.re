[%bs.raw {|require('./fog.css')|}];

let rowComponent = ReasonReact.statelessComponent("Fog");
let make = _children => {
  ...rowComponent,
  render: self =>
    <div className="fogwrapper">
      <div id="foglayer_01" className="fog">
        <div className="image01" />
        <div className="image02" />
      </div>
      <div id="foglayer_02" className="fog">
        <div className="image01" />
        <div className="image02" />
      </div>
      <div id="foglayer_03" className="fog">
        <div className="image01" />
        <div className="image02" />
      </div>
    </div>,
};