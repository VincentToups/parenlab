function [r] = equalquotedSexpression(input)
%
p = pbind(equalgreaterThanstring(''''), @lambdaminus82871);
r = funcall(p, input);
function [retvalminus82872] = lambdaminus82871(ignore)
%
retvalminus82872 = pbind(@equalsexpression, @lambdaminus82873);
function [retvalminus82874] = lambdaminus82873(theSexpression)
%
retvalminus82874 = preturn(cellArray(makeSymbol('quote'), theSexpression));

end

end

end