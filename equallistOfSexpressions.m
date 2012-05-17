function [r] = equallistOfSexpressions(input)
%
p = pbind(@equalopenParen, @lambdaminus77746);
r = p(input);
function [retvalminus77747] = lambdaminus77746(ignore)
%
retvalminus77747 = pbind(equalgreaterThanzeroOrMore(@equalsexpressionplusspaces), @lambdaminus77748);
function [retvalminus77749] = lambdaminus77748(contents)
%
retvalminus77749 = pbind(@equalcloseParen, @lambdaminus77750);
function [retvalminus77751] = lambdaminus77750(ignore)
%
retvalminus77751 = preturn(contents);

end

end

end

end