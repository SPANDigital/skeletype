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
  mutable randomWord: string,
  wordLane: lane,
};

type state = {
  time: int,
  intervalId: ref(option(Js.Global.intervalId)),
  skeletons: array(skeleton),
  words: array(word),
  input: string,
  refTextField: ref(option(Dom.element)),
};

type action =
  | Tick
  | ClearInput
  | ProcessInput(string)
  | AddWord(string, lane)
  | ReplaceWord(string, string)
  | SpawnSkeleton(int, lane)
  | KillSkeleton(int, lane)
  | FlagSkeletonAsDead(lane)
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

let fillWords = () => {
  let wordTop: word = {randomWord: "yo", wordLane: Top};
  let wordMiddle: word = {randomWord: "hey", wordLane: Middle};
  let wordBottom: word = {randomWord: "wow", wordLane: Bottom};
  [|wordTop, wordMiddle, wordBottom|];
};

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
      words: fillWords(),
      intervalId: ref(None),
      refTextField: ref(None),
      input: "",
    },
    didMount: self => {
      let intervalId =
        Some(Js.Global.setInterval(() => self.ReasonReact.send(Tick), 25));
      self.ReasonReact.state.intervalId := intervalId;
    },
    willUnmount: self =>
      switch (self.state.intervalId^) {
      | Some(id) => Js.Global.clearInterval(id)
      | None => ()
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
              /* Game has started */
              if (state.time === 0) {
                self.send(SpawnSkeleton(state.time, Top));
              } else {
                let lastArrayIndex = Array.length(state.skeletons) - 1;

                /* Every 100ms */
                if (state.time mod 100 === 0) {
                  let occupiedLanes =
                    ArrayLabels.map(s => s.lane, state.skeletons);
                  
                  let deployedLane = ref(Top); 

                  if (Array.length(occupiedLanes) !== 3) {
                    if (!Helpers.contains(Top, occupiedLanes)) {deployedLane := Top};
                    if (!Helpers.contains(Middle, occupiedLanes)) {deployedLane := Middle};
                    if (!Helpers.contains(Bottom, occupiedLanes)) {deployedLane := Bottom};
                    self.send(SpawnSkeleton(state.time, deployedLane^));
                  };
            
                };
              }
          ),
        )

      | ProcessInput(input) =>
        ReasonReact.UpdateWithSideEffects(
          {...state, input},
          (
            self =>
              if (Js.String.length(self.state.input) > 0) {
                let matchedWord =
                  Helpers.filter(
                    ~f=x => x.randomWord === self.state.input,
                    state.words,
                  );

                if (Array.length(matchedWord) > 0) {
                  let targetWord = matchedWord[0].randomWord;
                  let targetLane = matchedWord[0].wordLane;

                  self.send(ReplaceWord(targetWord, Dictionary.getRandomWord()));
                  self.send(KillSkeleton(self.state.time, targetLane));
                  self.send(ClearInput);
                };
              }
          ),
        )

      | ClearInput => ReasonReact.Update({...state, input: ""})

      | AddWord(randomWord, wordLane) =>
        let word: word = {randomWord, wordLane};
        let existingWords = state.words;
        switch (wordLane) {
        | Top => existingWords[0] = word
        | Middle => existingWords[1] = word
        | Bottom => existingWords[2] = word
        };
        ReasonReact.Update({...state, words: existingWords});

      | ReplaceWord(oldWord, newWord) =>
        let updatedWords = state.words;
        if (Array.length(updatedWords) > 0) {
          let entryToReplace =
            Helpers.find(~f=x => x.randomWord === oldWord, updatedWords);
          entryToReplace.randomWord = newWord;
        };
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
        skeleton.deathTime = state.time - skeleton.startTime;
        ReasonReact.UpdateWithSideEffects(
          {...state, skeletons: state.skeletons},
          (
            self => {
                setTimeout(() => self.send(FlagSkeletonAsDead(lane)), 1000);
              ();
            }
          ),
        );

      | FlagSkeletonAsDead(lane) =>
        let skeleton = Helpers.find(~f=x => x.lane === lane, state.skeletons);
        skeleton.status = Dead; 
        ReasonReact.UpdateWithSideEffects(
          {...state, skeletons: state.skeletons},
          (
            self => {
                setTimeout(() => self.send(RemoveSkeleton(lane)), 1000);
              ();
            }
          ),
        );

      | RemoveSkeleton(lane) =>
        let remainingSkeletons =
          Helpers.filter(~f=x => x.lane !== lane, state.skeletons);
        ReasonReact.Update({...state, skeletons: remainingSkeletons});
        
      },
    render: ({state, handle, send}) => {
      let {time, input, skeletons, words, _} = state;
      <div className="world">
        <div className="layout">
          <div className="header">
            <div className="begging">(ReasonReact.string("SKELETYPE"))</div>
            <div className="middle">(ReasonReact.string(string_of_int(time)))</div>
            <div className="end">(ReasonReact.string("X"))</div>
          </div>
          <div className="menu">
            (
              ReasonReact.array(
                Array.mapi(
                  (i, word) => <Word input randomWord=word.randomWord />,
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
              value=state.input
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