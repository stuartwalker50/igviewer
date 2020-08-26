import numpy as np
import matplotlib.pyplot as plt

# [timestamp,security,tradeCount, wins, winPnL, losses, lossPnL, cumPnL, maxDrawDown, maxLoss, maxGain] = np.genfromtxt('./results/histogram_L1:CS.D.USDJPY.MINI.IP.csv',delimiter=',',skip_header=1)  #this doesn't work, may as well just choose the cols you want to plot
# [timestamp,tradeCount, wins, winPnL, losses, lossPnL, cumPnL, maxDrawDown, maxLoss, maxGain] = np.genfromtxt('./results/histogram_L1:CS.D.USDJPY.MINI.IP.csv',delimiter=',',skip_header=1)

#ip=np.genfromtxt('./results/histogram_L1:CS.D.USDJPY.MINI.IP.csv',delimiter=',',names=True)
ip=np.genfromtxt('./results/histogram_L1:CS.D.EURUSD.MINI.IP.csv',delimiter=',',names=True)

# Compute pie slices
N = 20
#theta = np.linspace(0.0, 2 * np.pi, N, endpoint=False)
# theta = np.arange(0,48 / 2 * np.pi)
theta = ip['halfhour'] * 2 * np.pi / 48
radii = ip['cumPnL']
colors = plt.cm.viridis(radii / 10.)

# # https://stackoverflow.com/questions/56418087/how-to-plot-time-stamps-hhmm-on-python-matplotlib-clock-polar-plot
# ax = plt.subplot(221, projection='polar')
# ax.set_theta_direction(-1)     # Make the labels go clockwise
# ax.set_theta_offset(np.pi/2)   # Place Zero at Top
# ax.set_xticks(np.linspace(0, 2*np.pi, 24, endpoint=False))  #Set the circumference ticks
# ticks = ['12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM','8 AM','9 AM','10 AM','11 AM','12 PM', '1 PM', '2 PM', '3 PM', '4 PM',  '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM' ]  # set the label names
# ax.set_xticklabels(ticks)
# # plt.setp(ax.get_yticklabels(), visible=False)  # suppress the radial labels

# ax.bar(theta, ip['cumPnL'], width=0.1, bottom=0.0, color='green', alpha=0.5, label='cumPnL')
# ax.bar(theta, ip['lossPnL'], width=0.1, bottom=0.0, color='orange', alpha=0.5, label='lossPnL')
# ax.bar(theta, ip['winPnL'], width=0.1, bottom=0.0, color='blue', alpha=0.5, label='winPnL')
# # ax.bar(theta, ip['tradeCount'], width=0.1, bottom=0.0, color='blue', alpha=0.5)

ax2 = plt.subplot(121, projection='polar')
ax2.set_theta_direction(-1)     # Make the labels go clockwise
ax2.set_theta_offset(np.pi/2)   # Place Zero at Top
ax2.set_xticks(np.linspace(0, 2*np.pi, 24, endpoint=False))  #Set the circumference ticks
ticks = ['12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM','8 AM','9 AM','10 AM','11 AM','12 PM', '1 PM', '2 PM', '3 PM', '4 PM',  '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM' ]  # set the label names
ax2.set_xticklabels(ticks)
# plt.setp(ax2.get_yticklabels(), visible=False)  # suppress the radial labels

ax2.plot(theta, ip['cumPnL'], lw=5, color='green', alpha=0.5, marker='o', label='cumPnL')
ax2.plot(theta, ip['lossPnL'], lw=1, color='orange', alpha=0.5, marker='.', label='lossPnL')
ax2.plot(theta, ip['winPnL'], lw=1, color='blue', alpha=0.5, marker='.', label='winPnL')
ax2.plot(np.linspace(0,2*np.pi,48), np.linspace(0,0,48,False), lw=1, color='red', alpha=0.5)
ax2.legend()

ax3 = plt.subplot(122, projection='polar')
ax3.set_theta_direction(-1)     # Make the labels go clockwise
ax3.set_theta_offset(np.pi/2)   # Place Zero at Top
ax3.set_xticks(np.linspace(0, 2*np.pi, 24, endpoint=False))  #Set the circumference ticks
ticks = ['12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM','8 AM','9 AM','10 AM','11 AM','12 PM', '1 PM', '2 PM', '3 PM', '4 PM',  '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM' ]  # set the label names
ax3.set_xticklabels(ticks)
# plt.setp(ax2.get_yticklabels(), visible=False)  # suppress the radial labels

# ax2.plot(theta, ip['cumPnL'], lw=1, color='#ee8d18', alpha=0.5)
# ax3.plot(theta, ip['tradeCount'], lw=3, color='blue', alpha=0.5, label='tradeCount')
ax3.bar(theta, ip['tradeCount'], lw=3, color='blue', alpha=0.5, label='tradeCount')
ax3.legend()

# plt.savefig('results/chart/USDJPY.png', bbox_inches='tight')
plt.savefig('results/chart/USDJPY.png')
# plt.show()


