[%bs.raw {|require('./game.css')|}];
[@bs.module] external tileset : string = "../../assets/dugeon-tileset.png";
[@bs.val] external setTimeout : (unit => unit, int) => float = "setTimeout";
[@bs.val] external clearTimeout : float => unit = "clearTimeout";

type lane =
  | Top
  | Middle
  | Bottom;

type skeleton = {
  startTime: int,
  lane,
  mutable deathTime: int,
  mutable status: Skeleton.status,
};

type state = {
  time: int,
  intervalId: ref(option(Js.Global.intervalId)),
  skeletons: array(skeleton),
};

type action =
  | Tick
  | SpawnSkeleton(int, lane)
  | KillSkeleton(int, lane);

type gameState =
  | Menu
  | CountDown
  | Playing(lane)
  | Won
  | Lost;

let laneToInt = (lane: lane) =>
  switch (lane) {
  | Top => 1
  | Middle => 2
  | Bottom => 3
  };

let spawnSkeleton = (lane: lane) =>
  switch (lane) {
  | Top => Js.log("Top")
  | Middle => Js.log("Middle")
  | Bottom => Js.log("Bottom")
  };

let startTimer = self => {
  let intervalId =
    Some(Js.Global.setInterval(() => self.ReasonReact.send(Tick), 25));
  /* self.onUnmount(() => Js.Global.clearInterval(intervalId)); */
  self.ReasonReact.state.intervalId := intervalId;
};

let updateGameStatus = (status: gameState, self) =>
  switch (status) {
  | Menu => Js.log("Menu")
  | CountDown => Js.log("CountDown")
  | Won => Js.log("Won")
  | Lost => Js.log("Lost")
  | Playing(skeletonPosition) =>
    spawnSkeleton(skeletonPosition);
    startTimer(self);
  };

let gameComponent = ReasonReact.reducerComponent("Game");
let make = _children => {
  let click = (event, self) =>
    self.ReasonReact.send(KillSkeleton(self.state.time, Top));
  {
    ...gameComponent,
    initialState: () => {time: 0, skeletons: [||], intervalId: ref(None)},
    didMount: self => updateGameStatus(Playing(Top), self),
    reducer: (action, state) =>
      switch (action) {
      | Tick =>
        ReasonReact.UpdateWithSideEffects(
          {...state, time: state.time + 1},
          (
            self =>
              if (state.time === 0) {
                self.send(SpawnSkeleton(state.time, Top));
              } else {
                let lastArrayIndex = Array.length(state.skeletons) - 1;
                if (state.time
                    - state.skeletons[lastArrayIndex].startTime === 200) {
                  self.send(SpawnSkeleton(state.time, Middle));
                };
              }
          ),
        )
      | SpawnSkeleton(startTime, lane) =>
        let skeleton: skeleton = {
          startTime,
          deathTime: 0,
          lane,
          status: Walking,
        };
        let existingSkeletons = state.skeletons |> ArrayLabels.to_list;
        let updatedSkeletons =
          [skeleton, ...existingSkeletons] |> ArrayLabels.of_list;

        ReasonReact.Update({...state, skeletons: updatedSkeletons});
      | KillSkeleton(deathTime, lane) =>
        /* Flag skeleton to die! */
        let skeleton = Helpers.find(~f=x => x.lane === lane, state.skeletons);
        let arrIndex = laneToInt(skeleton.lane) - 1;
        let updatedSkeletons = state.skeletons;
        updatedSkeletons[arrIndex].status = Dying;
        updatedSkeletons[arrIndex].deathTime = state.time;

        ReasonReact.UpdateWithSideEffects(
          {...state, skeletons: updatedSkeletons},
          (
            self => {
              let id =
                setTimeout(
                  () => {
                    let updatedSkeletons =
                      Helpers.filter(
                        ~f=x => x.lane === lane,
                        state.skeletons,
                      );
                    ();
                    /* clearTimeout(id); */
                  },
                  2000,
                );
              ();
            }
          ),
        );
      },
    render: self => {
      let {time, skeletons, _} = self.state;

      <div className="world">
        <div className="layout">
          <div className="header" onClick=(self.handle(click))>
            (ReasonReact.string(string_of_int(time)))
          </div>
          <div className="menu" />
          <div className="content">
            (
              ReasonReact.array(
                Array.mapi(
                  (i, skeleton) =>
                    <Skeleton
                      lane=(laneToInt(skeleton.lane))
                      startTime=skeletons[i].startTime
                      deathTime=skeletons[i].deathTime
                      status=skeleton.status
                      time
                    />,
                  skeletons,
                ),
              )
            )
            <Row number="1" />
            <Row number="2" />
            <Row number="3" />
          </div>
          <div className="footer" />
        </div>
      </div>;
    },
  };
};