function [r] = equalvectorOfSexpressions(input)
%Parse a vector of s-expressions.
p = pbind(@equalopenSquare, @lambdaminus108408);
r = p(input);
function [retvalminus108409] = lambdaminus108408(ignore)
%
retvalminus108409 = pbind(equalgreaterThanzeroOrMore(@equalsexpressionplusspaces), @lambdaminus108410);
function [retvalminus108411] = lambdaminus108410(contents)
%
retvalminus108411 = pbind(@equalcloseSqare, @lambdaminus108412);
function [retvalminus108413] = lambdaminus108412(ignore)
%
retvalminus108413 = preturn([ cellArray(makeSymbol('vector')) contents ]);

end

end

end

end