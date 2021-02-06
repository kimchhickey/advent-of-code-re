open ReludeParse.Parser;

let input = Node_fs.readFileAsUtf8Sync("input/Year2020_Day7.txt");

let listToString = l => l->Belt.List.reduce("", (a, b) => a ++ b);

let wordParser = anyAlpha |> many <#> listToString;

let bagStringParser =
  str("bags, ")
  <|> str("bags")
  <|> str("bag, ")
  <|> str("bag.")
  <|> str("bag");

let bagParser: t(string) =
  wordParser
  <* wsStr
  >>= (
    adjective =>
      wordParser
      <* wsStr
      >>= (color => bagStringParser <#> (_ => adjective ++ " " ++ color))
  );

let bagsParser: t((int, string)) =
  (anyUnsignedInt <* wsStr, bagParser)
  |> mapTuple2((num, bag) => (num, bag));

let bagsOrNoBagsParser =
  str("no other bags.") <#> (_ => []) <|> many(bagsParser);

type bag = (string, array((int, string)));

let sentenceParser: t(bag) =
  (bagParser, wsStr *> str("contain") <* wsStr, bagsOrNoBagsParser)
  |> mapTuple3((bag, _, bags) => (bag, bags->Belt.List.toArray));

let bags =
  input
  ->Js.String2.trim
  ->Js.String2.split("\n")
  ->Belt.Array.map(s => runParser(s, sentenceParser));

let adjacencyList =
  bags->Belt.Array.map(result => {
    switch (result) {
    | Belt.Result.Ok(v) => v
    | Belt.Result.Error(_) => raise(Not_found)
    }
  });

let rec isConnected = (adjacencyList: array(bag), source, target, toVisit) => {
  let (_, children) =
    adjacencyList
    ->Belt.Array.keep(((vertex, _)) => vertex == source)
    ->Belt.Array.getExn(0);

  if (children->Belt.Array.some(((_, vertex)) => vertex == target)) {
    true;
  } else {
    let toVisit =
      children->Belt.Array.reduce(toVisit, (toVisit, vertex) =>
        toVisit->Belt.List.add(vertex)
      );

    switch (toVisit) {
    | [] => false
    | [(_, bag), ...xs] => isConnected(adjacencyList, bag, target, xs)
    };
  };
};

let rec totalWeight = (adjacencyList, source) => {
  let (_, children) =
    adjacencyList
    ->Belt.Array.keep(((vertex, _)) => vertex == source)
    ->Belt.Array.getExn(0);

  if (children->Belt.Array.length == 0) {
    0;
  } else {
    children->Belt.Array.reduce(0, (acc, (weight, vertex)) => {
      acc + weight + weight * totalWeight(adjacencyList, vertex)
    });
  };
};

// p1
Util.clog(
  adjacencyList
  ->Belt.Array.map(((vertex, _)) =>
      isConnected(adjacencyList, vertex, "shiny gold", [])
    )
  ->Belt.Array.keep(v => v)
  ->Belt.Array.length,
);

// p2
Util.clog(totalWeight(adjacencyList, "shiny gold"));
