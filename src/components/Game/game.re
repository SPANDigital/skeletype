[%bs.raw {|require('./game.css')|}];
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

type word = {
  randomWord: string,
};

type state = {
  time: int,
  intervalId: ref(option(Js.Global.intervalId)),
  skeletons: array(skeleton),
  words: array(word),
  text: string,
  refTextField: ref(option(Dom.element)),
};

type action =
  | Tick
  | ProcessInput(string)
  | AddWord(string)
  | SpawnSkeleton(int, lane)
  | KillSkeleton(int, lane)
  | RemoveSkeleton(lane);

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

let setSectionRef = (theRef, {ReasonReact.state}) =>
  state.refTextField := Js.Nullable.toOption(theRef);

let gameComponent = ReasonReact.reducerComponent("Game");
let make = _children => {
  let click = (event, self) =>
    self.ReasonReact.send(KillSkeleton(self.state.time, Top));
  let handleType = (event, self) => {
    let text = ReactDOMRe.domElementToObj(
                 ReactEventRe.Keyboard.target(event),
               )##value;
    self.ReasonReact.send(ProcessInput(text));
  };
  {
    ...gameComponent,
    initialState: () => {
      time: 0,
      skeletons: [||],
      words: [||],
      intervalId: ref(None),
      refTextField: ref(None),
      text: "",
    },
    didMount: self => {
      let intervalId = Some(Js.Global.setInterval(() => self.ReasonReact.send(Tick), 25));
      self.ReasonReact.state.intervalId := intervalId;
    },
    willUnmount: self => {
      switch (self.state.intervalId^) {
      | Some(id) => Js.Global.clearInterval(id)
      | None => ()
      }
    },
    didUpdate: ({oldSelf, newSelf}) =>
      switch (newSelf.state.refTextField^) {
      | Some(field) =>
        let node = ReactDOMRe.domElementToObj(field);
        ignore(node##focus());
      | _ => ()
      },
    reducer: (action, state) =>
      switch (action) {
      | Tick =>
        ReasonReact.UpdateWithSideEffects(
          {...state, time: state.time + 1},
          (
            self =>
              if (state.time === 0) {
                self.send(SpawnSkeleton(state.time, Top));
                self.send(AddWord("HERE"));
                self.send(AddWord("THERE"));
                self.send(AddWord("SOMMER"));
              } else {
                let lastArrayIndex = Array.length(state.skeletons) - 1;
                if (state.time
                    - state.skeletons[lastArrayIndex].startTime === 50) {
                  self.send(SpawnSkeleton(state.time, Middle));
                };
              }
          ),
        )
      | ProcessInput(text) => 
        ReasonReact.UpdateWithSideEffects(
          {...state, text},
          (
            self => ignore()
          )
        )        
      | AddWord(randomWord) =>
        let word: word = { randomWord: randomWord };
        let existingWords = state.words |> ArrayLabels.to_list;
        let updatedWords =
          [word, ...existingWords] |> ArrayLabels.of_list;

        ReasonReact.Update({...state, words: updatedWords});
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
        let skeleton = Helpers.find(~f=x => x.lane === lane, state.skeletons);
        skeleton.status = Dying;
        skeleton.deathTime = state.time;

        ReasonReact.UpdateWithSideEffects(
          {...state, skeletons: state.skeletons},
          (
            self => {
              let id =
                setTimeout(() => self.send(RemoveSkeleton(lane)), 2000);
              ();
            }
          ),
        );
      | RemoveSkeleton(lane) =>
        let remainingSkeletons = Helpers.filter(~f=x => x.lane !== lane, state.skeletons);
        ReasonReact.Update({...state, skeletons: remainingSkeletons});
      },
    render: ({state, handle, send}) => {
      let {time, text, skeletons, words, _} = state;
      <div className="world">
        <div className="layout">
          <div className="header" onClick=(handle(click))>
            (ReasonReact.string(string_of_int(time)))
          </div>
          <div className="menu">
          (
            ReasonReact.array(
              Array.mapi(
                (i, word) =>
                 <Word 
                 text=text
                 randomWord=word.randomWord />,
                words,
              ),
            )
          )
          </div>
          <div className="content">
            (
              ReasonReact.array(
                Array.mapi(
                  (i, skeleton) =>
                    <Skeleton
                      key={j|skeleton-$i|j}
                      lane=(laneToInt(skeleton.lane))
                      startTime=skeleton.startTime
                      deathTime=skeleton.deathTime
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
          <div className="footer">
            <input
              ref=(handle(setSectionRef))
              value=state.text
              onChange=(
                event =>
                  send(
                    ProcessInput(
                      ReactDOMRe.domElementToObj(
                        ReactEventRe.Form.target(event),
                      )##value,
                    ),
                  )
              )
            />
          </div>
        </div>
      </div>;
    },
  };
};