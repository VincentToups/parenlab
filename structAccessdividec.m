function [b] = structAccessdividec(field)
%Return a function which returns the FIELD of the struct.
b = @lambdaminus60154;
function [retvalminus60155] = lambdaminus60154(theStruct)
%
retvalminus60155 = [ theStruct.(field) ];

end

end