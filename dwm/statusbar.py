#!/usr/bin/env python
"""Simple status bar script for DWM."""

import glob
import os
import re
import time
import datetime
import subprocess

GREEN = u"\x05"
WHITE = u"\x01"

class BaseWidget(object):
    """The base status bar widget."""
    def get_icon(self):
        """Override to return a custom icon for this widget."""
        raise NotImplementedError

    def get_status_text(self):
        """Override to return the status text for this widget."""
        raise NotImplementedError

    def prepare(self):
        """Prepares the widget for the next iteration

        This is where most of the widget logic should take place. If the widget
        needs to gather any data from system calls, then it should do so here
        and save the parsed data as member variables. `get_icon()` and
        `get_status_text()` can should then read these member variables to
        construct their output.

        This method should return a boolean which determines whether the widget
        will be included in the statusbar in the next iteration.

        Returns:
            bool: if True, then this widget will be included in the statusbar in
                the next iteration.

        """
        return True

    def __unicode__(self):
        return GREEN + self.get_icon() + WHITE + self.get_status_text()

    def __str__(self):
        return unicode(self).encode("utf-8")

class ClockWidget(BaseWidget):
    """A basic clock widget that shows the current date and time."""
    def get_icon(self):
        return u"\uE015"

    def get_status_text(self):
        return datetime.datetime.now().strftime("%a %b %d %r")

class MemUsageWidget(BaseWidget):
    """Displays memory usage information in megabytes and as a percent."""
    def __init__(self):
        super(MemUsageWidget, self).__init__()

        self.mem_used_gb = None
        self.mem_used_perc = None

    def get_icon(self):
        return u"\uE020"

    def get_status_text(self):
        return "%d%% (%.2fG)" % (self.mem_used_perc, self.mem_used_gb)

    def prepare(self):
        """Reads /proc/meminfo to get mem usage data."""
        with open("/proc/meminfo") as f:
            # The first 4 lines of mem_info give us the info we want
            mem_total_kb = int(f.readline().split()[1])
            mem_free_kb = int(f.readline().split()[1])
            buffers_kb = int(f.readline().split()[1])
            cached_kb = int(f.readline().split()[1])

        mem_used_kb = mem_total_kb - mem_free_kb - buffers_kb - cached_kb

        self.mem_used_gb = mem_used_kb / (1024.0 ** 2)
        self.mem_used_perc = 100 * mem_used_kb / mem_total_kb

        return True

class CPUTempWidget(BaseWidget):
    """Displays the temperatures of the CPU cores."""
    def __init__(self):
        super(CPUTempWidget, self).__init__()

        self.coretemp_paths = glob.glob(
            "/sys/bus/platform/drivers/coretemp/coretemp.*")

        self.temp_readings = []

    def get_icon(self):
        return u"\uE01c"

    def get_status_text(self):
        return " / ".join("%dC" % reading for reading in self.temp_readings)

    def prepare(self):
        """Reads temp1_input in each of the coretemp directories."""
        if len(self.coretemp_paths) == 0:
            return False

        self.temp_readings = []
        for path in self.coretemp_paths:
            with open(os.path.join(path, "temp1_input")) as f:
                self.temp_readings.append(int(f.read()) / 1000)

        return True

class CPUUsageWidget(BaseWidget):
    """Displays the percent utilization per CPU core over the last interval."""
    def __init__(self):
        super(CPUUsageWidget, self).__init__()

        self.prev_used, self.prev_total = [], []
        self.cur_used, self.cur_total = [], []

        # Execute one iteration of `prepare()` immediately so we have both
        # `prev_` and `cur_` values in the next iteration.
        self.prepare()

    def get_icon(self):
        return u"\uE026"

    def get_status_text(self):
        delta_used = [(c - p) for c, p in zip(self.cur_used, self.prev_used)]
        delta_total = [(c - p) for c, p in zip(self.cur_total, self.prev_total)]

        percent_used = []
        for used, total in zip(delta_used, delta_total):
            percent_used.append(100 * used / total if total > 0 else 0)

        return " / ".join("%2d%%" % percent for percent in percent_used)

    def prepare(self):
        """Reads /proc/stat to get CPU usage data."""
        self.prev_used, self.prev_total = self.cur_used, self.cur_total
        self.cur_used, self.cur_total = [], []
        with open("/proc/stat") as f:
            for line in f:
                if not line.startswith("cpu"):
                    break
                elif not re.match("cpu\d", line):
                    continue
                else:
                    words = line.split()
                    user_time = int(words[1])
                    nice_time = int(words[2])
                    system_time = int(words[3])
                    idle_time = int(words[4])

                    self.cur_used.append(user_time + nice_time + system_time)
                    self.cur_total.append(self.cur_used[-1] + idle_time)

        return True

class NetSpeedWidget(BaseWidget):
    """Displays the current download and upload speed in kb/s."""
    def __init__(self):
        super(NetSpeedWidget, self).__init__()

        self.prev_down, self.prev_up = None, None
        self.cur_down, self.cur_up = None, None

        # Execute one iteration of `prepare()` immediately so we have both
        # `prev_` and `cur_` values in the next iteration.
        self.prepare()

    def get_icon(self):
        return (u"\uE061", u"\uE060")

    def get_status_text(self):
        if ((self.cur_down is None and self.cur_up is None) or
            (self.prev_down is None and self.prev_up is None)):
            speed_down, speed_up = 0, 0
        else:
            speed_down = (self.cur_down - self.prev_down) / 1024.0
            speed_up = (self.cur_up - self.prev_up) / 1024.0

        return (u"%4.1fk/s" % speed_down, "%4.1fk/s" % speed_up)

    def prepare(self):
        """Returns the current number of bytes that have been transferred.

        Returns:
            tuple (down_bytes, up_bytes) where down_bytes is the total number of
            bytes that have been downloaded and up_bytes is the total number of
            bytes that have been uploaded.
        """
        self.prev_down, self.prev_up = self.cur_down, self.cur_up

        # Get the network interface currently associated with the default route
        net_iface = None
        with open("/proc/net/route") as f:
            for line in f:
                words = line.split()
                try:
                    destination = int(words[1], 16)
                except ValueError:
                    continue

                if destination == 0:
                    net_iface = words[0]
                    break

        if not net_iface:
            return False

        with open("/sys/class/net/%s/statistics/rx_bytes" % net_iface) as f:
            self.cur_down = int(f.read())

        with open("/sys/class/net/%s/statistics/tx_bytes" % net_iface) as f:
            self.cur_up = int(f.read())

        return True

    def __unicode__(self):
        """Override to show both down and up icons next to their speeds."""
        icon_down, icon_up = self.get_icon()
        speed_down, speed_up = self.get_status_text()

        return u"%s%s%s%s %s%s%s%s" % (GREEN, icon_down, WHITE, speed_down,
                                       GREEN, icon_up, WHITE, speed_up)

class WirelessInfoWidget(BaseWidget):
    """Displays the essid and signal strength of the current wireless network."""
    def __init__(self):
        super(WirelessInfoWidget, self).__init__()

        self.net_iface = None
        self.essid = None
        self.signal_strength = None

    def get_icon(self):
        return u"\uE048"

    def get_status_text(self):
        return "%s %d%%" % (self.essid, self.signal_strength)

    def prepare(self):
        try:
            p = subprocess.Popen(["iwgetid"], stdout=subprocess.PIPE)
        except OSError:
            return False

        match = re.match("^(\w+)\s+ESSID:\"(.*)\"$", p.stdout.read())
        if not match:
            return False

        self.net_iface, self.essid = match.groups()
        with open("/sys/class/net/%s/wireless/link" % self.net_iface) as f:
            try:
                self.signal_strength = int(f.read())
            except IOError:
                return False

        return True

def main():
    """Main loop."""
    widgets = [
        WirelessInfoWidget(),
        NetSpeedWidget(),
        CPUUsageWidget(),
        CPUTempWidget(),
        MemUsageWidget(),
        ClockWidget()
    ]

    while True:
        status_text = " |".join([unicode(w) for w in widgets if w.prepare()])
        subprocess.Popen(["xsetroot", "-name", status_text])
        time.sleep(1)

if __name__ == "__main__":
    main()
