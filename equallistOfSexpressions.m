function [r] = equallistOfSexpressions(input)
%
p = pbind(@equalopenParen, @lambdaminus108402);
r = p(input);
function [retvalminus108403] = lambdaminus108402(ignore)
%
retvalminus108403 = pbind(equalgreaterThanzeroOrMore(@equalsexpressionplusspaces), @lambdaminus108404);
function [retvalminus108405] = lambdaminus108404(contents)
%
retvalminus108405 = pbind(@equalcloseParen, @lambdaminus108406);
function [retvalminus108407] = lambdaminus108406(ignore)
%
retvalminus108407 = preturn(contents);

end

end

end

end