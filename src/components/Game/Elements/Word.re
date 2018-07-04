

let rowComponent = ReasonReact.statelessComponent("Word");
let make = (~randomWord, ~text, _children) => {
  ...rowComponent,
  render: _self => {
    let normalWord = ref(randomWord);  
    let matchedWord = ref(text);
    if(text !== "" && Js.String.startsWith(text, randomWord)){
        let splitWord = Js.String.split(text, randomWord)[1];
        matchedWord := text;
        normalWord := splitWord;
    }; 
    <div className="word-box">
        <div className="word">
            <span className="matched"> (ReasonReact.string(matchedWord^))</span>
            <span className="normal"> (ReasonReact.string(normalWord^))</span>
        </div>
    </div>;
  },
};