function [r] = equalquotedSexpression(input)
%
p = pbind(equalgreaterThanstring(''''), @lambdaminus77732);
r = funcall(p, input);
function [retvalminus77733] = lambdaminus77732(ignore)
%
retvalminus77733 = pbind(@equalsexpression, @lambdaminus77734);
function [retvalminus77735] = lambdaminus77734(theSexpression)
%
retvalminus77735 = preturn(cellArray(makeSymbol('quote'), theSexpression));

end

end

end