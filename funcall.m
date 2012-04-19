function varargout=funcall(f,varargin)
try
  varargout{1:nargout} = f(varargin{:});
catch

  if nargout > 0
    names = '[';
    for i=1:nargout
      name = sprintf('n%d',i);
      names = [names name ','];
    end
    names(end) = ']';
    %try
    eval(sprintf('%s = f(varargin{:});', names));
    %catch
    %  keyboard
    %end
    varargout = {};
    for i=1:nargout
      varargout{i} = eval(sprintf('n%d;',i));
    end
  else
    f(varargin{:}); 
  end
end
