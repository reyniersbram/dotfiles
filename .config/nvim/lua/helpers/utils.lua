local M = {}

M.prequire = function (module)
    return pcall(require, module)
end

return M
