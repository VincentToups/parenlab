function [r] = equalsexpression(input)
%Parse an s-expression.
compound = equalgreaterThanor(@equalquotedSexpression, @equalsharpQuotedSexpression, @equalsymbol, @equalkeyword, @equalnumber, @equallistOfSexpressions, @equalvectorOfSexpressions, @equallispString);
r = compound(input);

end