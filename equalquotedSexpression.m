function [r] = equalquotedSexpression(input)
%
p = pbind(equalgreaterThanstring(''''), @lambdaminus108388);
r = funcall(p, input);
function [retvalminus108389] = lambdaminus108388(ignore)
%
retvalminus108389 = pbind(@equalsexpression, @lambdaminus108390);
function [retvalminus108391] = lambdaminus108390(theSexpression)
%
retvalminus108391 = preturn(cellArray(makeSymbol('quote'), theSexpression));

end

end

end