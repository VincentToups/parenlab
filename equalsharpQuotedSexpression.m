function [r] = equalsharpQuotedSexpression(input)
%
p = pbind(equalgreaterThanstring('#'''), @lambdaminus82875);
r = funcall(p, input);
function [retvalminus82876] = lambdaminus82875(ignore)
%
retvalminus82876 = pbind(@equalsexpression, @lambdaminus82877);
function [retvalminus82878] = lambdaminus82877(theSexpression)
%
retvalminus82878 = preturn(cellArray(makeSymbol('function'), theSexpression));

end

end

end