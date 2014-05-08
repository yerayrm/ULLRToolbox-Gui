from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys
import interface.mainwindow as mw
import custom_mainwindow as cm

class KeyPressEater(QtCore.QObject):
	def eventFilter(self, widget, event):
		print('return 0')
		if event.type() == QtCore.QEvent.KeyPress:
			key = event.key()
			if key == QtCore.Qt.Key_Return:
				print('return')
				return True
		return QtGui.QWidget.eventFilter(self, widget, event)