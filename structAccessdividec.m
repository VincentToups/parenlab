function [b] = structAccessdividec(field)
%Return a function which returns the FIELD of the struct.
b = @lambdaminus77715;
function [retvalminus77716] = lambdaminus77715(theStruct)
%
retvalminus77716 = [ theStruct.(field) ];

end

end