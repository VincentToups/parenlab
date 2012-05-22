function [r] = equalsharpQuotedSexpression(input)
%
p = pbind(equalgreaterThanstring('#'''), @lambdaminus108392);
r = funcall(p, input);
function [retvalminus108393] = lambdaminus108392(ignore)
%
retvalminus108393 = pbind(@equalsexpression, @lambdaminus108394);
function [retvalminus108395] = lambdaminus108394(theSexpression)
%
retvalminus108395 = preturn(cellArray(makeSymbol('function'), theSexpression));

end

end

end