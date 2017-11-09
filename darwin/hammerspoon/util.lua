local util = {}

-- Returns a table with the x and y coordinates of the top left of the given
-- rect table.
--
-- Args:
--     rect, table: a rect table {x = int, y = int, w = int, h = int}
--
-- Returns:
--     a point table {x = int, y = int} indicating the top left point
function util.getRectTopLeft(rect)
    return {
        x = rect.x,
        y = rect.y
    }
end

-- Returns a table with the x and y coordinates of the center of the given rect
-- table.
--
-- Args:
--     rect, table: a rect table {x = int, y = int, w = int, h = int}
--
-- Returns:
--     a point table {x = int, y = int} indicating the center point of the rect
function util.getRectCenter(rect)
    return {
        x = rect.x + rect.w / 2,
        y = rect.y + rect.h / 2
    }
end

-- One-based modulo function equivalent to `a % b`, except this will always
-- return a value in the range [1, b].
--
-- Args:
--     a, int: the dividend in the express a % b
--     b, int: the divisor in the express a % b
--
-- Returns:
--     the result of a % b using one-based indexing
function util.mod(a, b)
    return (a - 1) % b + 1
end

-- Emulates the ternary operator.
--
-- Args:
--     cond, bool: a condition to evaluate
--     trueOutcome, obj: the value to be returned if the condition evaluates to
--         true
--     falseOutcome, obj: the value to be returned if the condition evaluates to
--         false
--
-- Returns:
--     evaluates the expression `cond ? trueOutcome : falseOutcome`
function util.ternary(cond, trueOutcome , falseOutcome)
    if cond then
        return trueOutcome
    else
        return falseOutcome
    end
end

-- Returns the center of the given list of point / rect tables
function util.getCenterOfPoints(points)
    local pointsSum = {x = 0, y = 0}

    for i = 1, #points do
        pointsSum.x = pointsSum.x + points[i].x
        pointsSum.y = pointsSum.y + points[i].y
    end

    return {
        x = pointsSum.x / #points,
        y = pointsSum.y / #points
    }
end


-- Returns true if the given rect objects are approximate matches.
--
-- Please refer to the `isApprox` match for implementation details about how
-- we determine whether two rects are approximate matches.
function util.isRectsApproxMatch(rect1, rect2)
    return (util.isApprox(rect1.x, rect2.x) and
            util.isApprox(rect1.x + rect1.w, rect2.x + rect2.w) and
            util.isApprox(rect1.y, rect2.y) and
            util.isApprox(rect1.y + rect1.h, rect1.y + rect2.h))
end

-- Returns true if the given rect objects are mostly overlapping.

-- We define an approximate match as one where at least half of the area of the
-- first rect object is within the bounds of the second rect object.
function util.isRectsOverlap(rect1, rect2)
    local overlapCoords = {
        x1 = math.max(rect1.x, rect2.x),
        y1 = math.max(rect1.y, rect2.y),
        x2 = math.min(rect1.x + rect1.w, rect2.x + rect2.w),
        y2 = math.min(rect1.y + rect1.h, rect2.y + rect2.h)
    }

    if (overlapCoords.x2 - overlapCoords.x1 < 0 or
            overlapCoords.y2 - overlapCoords.y1 < 0) then
        return false
    end

    local overlapArea = ((overlapCoords.x2 - overlapCoords.x1) *
                         (overlapCoords.y2 - overlapCoords.y1))
    local rect1Area = rect1.w * rect1.h

    return overlapArea / rect1Area > 0.5
end

-- Return true if the two value are within a certain range of each other.

-- The default tolerance is set to 20 units. Here, a unit corresponds to a
-- pixel.
function util.isApprox(value1, value2, tolerance)
    tolerance = tolerance or 20
    return math.max(value1, value2) - math.min(value1, value2) < tolerance;
end

-- Creates hotkey bindings for all hotkeys specified in the config.
--
-- The `configs` argument should be a list of argument tables that will be
-- passed directly into `hs.hotkey.bind`.
--
-- Args:
--     configs, table: a list of argument tables to be passed to `hs.hotkey.bind`
function util.bindAll(configs)
    for i, config in ipairs(configs) do
        hs.hotkey.bind(table.unpack(config))
    end
end

-- Implements the `any` function. Returns `true` if the predicate returns true
-- for any object in the given table.
function util.any(coll, pred)
    for _, el in ipairs(coll) do
        if pred(el) then
            return true
        end
    end
    return false
end

-- Implements the `find` function. Returns the object in the table that returns
-- `true` when applying the predicate.
function util.find(coll, pred)
    for _, el in ipairs(coll) do
        if pred(el) then
            return el
        end
    end
    return nil
end

return util
