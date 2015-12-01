--
-- Adds flux functionality for modifying screen gamma and brightness.
--
local flux = {}

-- A list of 2-tuples {brightness, colortemp} specifying the different flux
-- levels where `brightness` is in the range [0, 1] and `colortemp` is in [1000,
-- 65000]. The default brightness / colortemp setting is {1, 6500}.
flux.SETTINGS = {
    {0.75, 5000},
    {0.9, 5800},
    {1, 6500}
}

local CUR_LEVEL = #flux.SETTINGS

-- RGB values for whitepoint colors at different color temperatures.
-- This table is the same one used in Redshift:
-- https://github.com/jonls/redshift/blob/master/README-colorramp
local WHITEPOINT_COLORS = {
    -- 1000k
    {1.00000000,  0.18172716,  0.00000000},
    -- 1100k
    {1.00000000,  0.25503671,  0.00000000},
    -- 1200k
    {1.00000000,  0.30942099,  0.00000000},
    {1.00000000,  0.35357379,  0.00000000},
    {1.00000000,  0.39091524,  0.00000000},
    {1.00000000,  0.42322816,  0.00000000},
    {1.00000000,  0.45159884,  0.00000000},
    {1.00000000,  0.47675916,  0.00000000},
    {1.00000000,  0.49923747,  0.00000000},
    {1.00000000,  0.51943421,  0.00000000},
    {1.00000000,  0.54360078,  0.08679949},
    {1.00000000,  0.56618736,  0.14065513},
    {1.00000000,  0.58734976,  0.18362641},
    {1.00000000,  0.60724493,  0.22137978},
    {1.00000000,  0.62600248,  0.25591950},
    {1.00000000,  0.64373109,  0.28819679},
    {1.00000000,  0.66052319,  0.31873863},
    {1.00000000,  0.67645822,  0.34786758},
    {1.00000000,  0.69160518,  0.37579588},
    {1.00000000,  0.70602449,  0.40267128},
    {1.00000000,  0.71976951,  0.42860152},
    {1.00000000,  0.73288760,  0.45366838},
    {1.00000000,  0.74542112,  0.47793608},
    {1.00000000,  0.75740814,  0.50145662},
    {1.00000000,  0.76888303,  0.52427322},
    {1.00000000,  0.77987699,  0.54642268},
    {1.00000000,  0.79041843,  0.56793692},
    {1.00000000,  0.80053332,  0.58884417},
    {1.00000000,  0.81024551,  0.60916971},
    {1.00000000,  0.81957693,  0.62893653},
    {1.00000000,  0.82854786,  0.64816570},
    {1.00000000,  0.83717703,  0.66687674},
    {1.00000000,  0.84548188,  0.68508786},
    {1.00000000,  0.85347859,  0.70281616},
    {1.00000000,  0.86118227,  0.72007777},
    {1.00000000,  0.86860704,  0.73688797},
    {1.00000000,  0.87576611,  0.75326132},
    {1.00000000,  0.88267187,  0.76921169},
    {1.00000000,  0.88933596,  0.78475236},
    {1.00000000,  0.89576933,  0.79989606},
    {1.00000000,  0.90198230,  0.81465502},
    {1.00000000,  0.90963069,  0.82838210},
    {1.00000000,  0.91710889,  0.84190889},
    {1.00000000,  0.92441842,  0.85523742},
    {1.00000000,  0.93156127,  0.86836903},
    {1.00000000,  0.93853986,  0.88130458},
    {1.00000000,  0.94535695,  0.89404470},
    {1.00000000,  0.95201559,  0.90658983},
    {1.00000000,  0.95851906,  0.91894041},
    {1.00000000,  0.96487079,  0.93109690},
    {1.00000000,  0.97107439,  0.94305985},
    {1.00000000,  0.97713351,  0.95482993},
    {1.00000000,  0.98305189,  0.96640795},
    {1.00000000,  0.98883326,  0.97779486},
    {1.00000000,  0.99448139,  0.98899179},
    -- 6500k (default)
    {1.00000000,  1.00000000,  1.00000000}
}

local function setLevel(level)
    CUR_LEVEL = level

    -- local brightnessDelta = (
    --     (flux.MAX_BRIGHTNESS - flux.MIN_BRIGHTNESS) /
    --     (flux.NUM_LEVELS - 1))
    -- local targetBrightness = flux.MIN_BRIGHTNESS + brightnessDelta * CUR_LEVEL

    -- local colorTempDelta = (
    --     (flux.MAX_COLOR_TEMP - flux.MIN_COLOR_TEMP) /
    --     (flux.NUM_LEVELS - 1))
    -- local targetColorTemp = flux.MIN_COLOR_TEMP + colorTempDelta * CUR_LEVEL

    local targetBrightness, targetColorTemp = table.unpack(flux.SETTINGS[level])

    -- Get the index of the target color temp in the whitepoint colors
    -- table. The first index in the table corresponds to 1000k, and the last
    -- corresponds to 6500k.
    local targetColorTempIx = math.floor((targetColorTemp - 1000) / 100)
    local targetWhitepoint = WHITEPOINT_COLORS[targetColorTempIx + 1]

    local targetWhitepointWithBrightness = {
        red = targetWhitepoint[1] * targetBrightness,
        green = targetWhitepoint[2] * targetBrightness,
        blue = targetWhitepoint[3] * targetBrightness
    }

    -- We don't want to do anything fancy w/ the blackpoint so we always want to
    -- leave it the same.
    local targetBlackpoint = {
        alpha = 1,
        red = 0,
        green = 0,
        blue = 0
    }

    for i, screen in ipairs(hs.screen.allScreens()) do
        screen:setGamma(targetWhitepointWithBrightness, targetBlackpoint)
    end
end

-- Decreases the current brightness / gamma level.
function flux.decreaseLevel()
    if CUR_LEVEL > 1 then
        setLevel(CUR_LEVEL - 1)
    end
end

-- Increases the current brightness / gamma level.
function flux.increaseLevel()
    if CUR_LEVEL < #flux.SETTINGS then
        setLevel(CUR_LEVEL + 1)
    end
end

return flux
