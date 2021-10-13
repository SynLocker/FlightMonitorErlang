from string import Template
import random

for id in range(100):
    x = random.randint(-1000, 1000)
    y = random.randint(-1000, 1000)
    angle = random.randint(0, 359)
    speed = round(random.random(), 2)
    current_plane = Template('[{plane_id, <<"flight:$id">>},{x, $x}, {y, $y}, {angle, $angle}, {speed, $speed}],')
    print(current_plane.substitute(x = x, y = y, angle = angle, speed = speed, id = id))