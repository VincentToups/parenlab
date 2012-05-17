function [r] = equallistOfSexpressions(input)
%
p = pbind(@equalopenParen, @lambdaminus82885);
r = p(input);
function [retvalminus82886] = lambdaminus82885(ignore)
%
retvalminus82886 = pbind(equalgreaterThanzeroOrMore(@equalsexpressionplusspaces), @lambdaminus82887);
function [retvalminus82888] = lambdaminus82887(contents)
%
retvalminus82888 = pbind(@equalcloseParen, @lambdaminus82889);
function [retvalminus82890] = lambdaminus82889(ignore)
%
retvalminus82890 = preturn(contents);

end

end

end

end