---------------------------------------------------------------------------
-- @author Calvin Young <calvinwyoung@gmail.com>
--
-- Custom tiling layout that arranges master windows in columns. Slave windows
-- get added to an additional column, where they're stacked horizontally.
---------------------------------------------------------------------------

-- Grab environment we need
local ipairs = ipairs
local math = math
local tag = require("awful.tag")

--- Tiled layouts module for awful
module("custom.layouts.tile")

-- Arrange a group of clients in the specified clients list within the specified workarea.
local function tile_group(workarea, clients, start_ix, end_ix, is_vert)
    local offset_x, offset_y = workarea.x, workarea.y
    for i=start_ix, end_ix do
        local geom = {x=offset_x, y=offset_y}

        if is_vert then
            geom["height"] = workarea.height
            if i ~= end_ix then
                geom["width"] = math.floor(workarea.width / (end_ix - start_ix + 1))
            else
                geom["width"] = workarea.x + workarea.width - offset_x
            end
            offset_x = offset_x + geom.width
        else
            geom["width"] = workarea.width
            if i ~= end_ix then
                geom["height"] = math.floor(workarea.height / (end_ix - start_ix + 1))
            else
                geom["height"] = workarea.y + workarea.height - offset_y
            end
            offset_y = offset_y + geom.height
        end

        clients[i]:geometry(geom)
    end
end

-- Main tile function to arrange all clients on the screen.
local function tile(param, orientation)
    orientation = orientation or "right"

    local t = tag.selected(param.screen)
    local clients = param.clients
    local nmaster = math.min(tag.getnmaster(t), #clients)
    local workarea = param.workarea

    -- Get the minimum client width from the client size hints
    local min_client_width = 0
    for i, client in ipairs(clients) do
        min_client_width = (client.size_hints["min_width"] or
                            client.size_hints["base_width"] or 0)
        min_client_width = min_client_width + client.border_width * 2
    end

    -- Now compute the dimensions for the master and slave workareas
    local master_width = workarea.width
    if #clients - nmaster > 0 then
        master_width = math.max(
            math.ceil(workarea.width * tag.getmwfact(t)),
            min_client_width * nmaster)
    end
    local slave_width = workarea.width - master_width

    local master_workarea = {
        width=master_width,
        height=workarea.height,
        x=(orientation == "right" and workarea.x or workarea.x + slave_width),
        y=workarea.y
    }
    local slave_workarea = {
        width=slave_width,
        height=workarea.height,
        x=(orientation == "right" and workarea.x + master_width or workarea.x),
        y=workarea.y
    }

    -- Finally, we can tile the master and slave windows
    tile_group(master_workarea, clients, 1, nmaster, true)
    tile_group(slave_workarea, clients, nmaster + 1, #clients, false)
end

--- The main tile layout with the slave stack on the right. The is the default
--- configuration.
right = {}
right.name = "tile"
right.arrange = tile

--- The main tile layout with the slave stack on the left.
left = {}
left.name = "tileleft"
left.arrange = function(p)
    return tile(p, "left")
end

name = right.name
arrange = right.arrange
