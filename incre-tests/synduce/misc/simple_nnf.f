Inductive Formula = flit Bool | fand {Formula, Formula} | forr {Formula, Formula} | fnot Formula;
Inductive NnfFormula = nfneglit Bool | nflit Bool | nfand {NnfFormula, NnfFormula} | nfor {NnfFormula, NnfFormula};

spec = fix (
  \f: Formula -> Bool. \x: Formula.
  match x with
    flit b -> b
  | fand {b1, b2} -> and (f b1) (f b2)
  | forr {b1, b2} -> or (f b1) (f b2)
  | fnot b -> if (f b) then false else true
  end
);

repr = fix (
  \f: NnfFormula -> Compress Formula. \x: NnfFormula.
  match x with
    nflit b -> flit b
  | nfneglit b -> fnot (flit b)
  | nfand {b1, b2} -> fand {f b1, f b2}
  | nfor {b1, b2} -> forr {f b1, f b2}
  end
);

main = \xs: NnfFormula. spec (repr xs);