function [r] = equalsharpQuotedSexpression(input)
%
p = pbind(equalgreaterThanstring('#'''), @lambdaminus77736);
r = funcall(p, input);
function [retvalminus77737] = lambdaminus77736(ignore)
%
retvalminus77737 = pbind(@equalsexpression, @lambdaminus77738);
function [retvalminus77739] = lambdaminus77738(theSexpression)
%
retvalminus77739 = preturn(cellArray(makeSymbol('function'), theSexpression));

end

end

end