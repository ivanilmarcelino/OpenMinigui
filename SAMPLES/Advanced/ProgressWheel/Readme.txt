Progress Wheel is a round progress control with customizable colors, shape and gradient.

property ColorDoneMin: RGB Color;
Color of Min side of progress wheel. This property is used only if GradientMode property is not None.

property ColorDoneMax: RGB Color;
Color of Max side of progress wheel. This property is also used if GradientMode is None as a color of done part of progress bar.

property ColorRemain: RGB Color;
Color of remain part of progress bar.

property ColorInner: RGB Color;
Color of inner part of progress wheel.

property InnerSize: Integer;
Size of inner part of progress wheel in percents of progress wheel diameter. Possible value is from 0 to 99.

property StartAngle: Integer;
Start angle of done part in degrees. 0 is 12 o'clock. Possible value is from 0 to 359.

property Min: Integer;
Lower limit of the range of possible positions.

property Max: Integer;
Upper limit of the range of possible positions.

property Position: Integer;
Current position of the progress wheel.

property ShowText: Boolean;
If this property is true, the progress text is shown in the center of progress wheel.

property GradientMode: Integer;
Mode of progress wheel gradient. If the value is None, gradient is off, and only ColorDoneMax is used for drawing of gradient wheel. If the GradientMode is Position the flat color of gradient wheel depends from Position value. If GradientMode is Angle, the conic gradient from ColorDoneMin to ColorDoneMax is used for drawing done part of progress wheel.
