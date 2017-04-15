function module(modname,...)
end

require "util"
util = {}
util.table = {}
util.table.deepcopy = table.deepcopy
util.multiplystripes = multiplystripes
