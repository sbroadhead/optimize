#
# visualize.py
# 
# The main entry point for the visualizer
#

from vector import *
from pygame.locals import *
import pygame
import sys
import json

WINDOW_WIDTH = 1280
WINDOW_HEIGHT = 720

class Problem(object):
    """Problem definition"""

    def __init__(self, filename):
        self.load_from_file(filename)

    def load_from_file(self, filename):
        with open(filename, 'r') as f:
            arr = json.load(f)

        self.radius = arr.get('radius', 1.0)
        self.mass = arr.get('mass', 1.0)
        self.start_position = Vector(*arr.get('start_position', [0.0, 0.0]))
        self.start_direction = Vector(*arr.get('start_direction', [1.0, 0.0]))
        self.end_position = Vector(*arr.get('end_position', [0.0, 0.0]))
        self.sensor_degree = arr.get('sensor_degree', 0.5)
        
        self.polygons = [[Vector(*x) for x in p] for p in arr.get('polygons', [])]

        borders = arr.get('borders', [[-100, -100], [-100, 100], [100, 100], [100, -100]])
        for i in xrange(0, 4):
            p1 = borders[i]
            p2 = borders[(i+1)%4]
            perp = normalized(Vector(*(-(p2[1]-p1[1]), p2[0]-p1[0])))
            
            self.polygons.append([Vector(*p1), Vector(*p2), Vector(*p2)+perp, Vector(*p1)+perp])

class Ship(object):
    """Ship state"""

    def __init__(self):
        self.radius = 1.0
        self.mass = 1.0
        self.position = Vector(0.0, 0.0)
        self.direction = Vector(1.0, 0.0)
        self.velocity = Vector(0.0, 0.0)
        self.angular_velocity = 0.0
        self.left_thrust = 0.0
        self.right_thrust = 0.0
        self.step_length = 0.1

        self.sensor_data = []
        self.target = None

    def step(self, dt):
        t = 0
        while t < dt:
            if t+self.step_length <= dt:
                h = self.step_length
            else:
                h = dt-t

            self.direction = normalized(self.direction)
            domega = 2*(self.left_thrust - self.right_thrust)/(self.radius * self.mass)
            new_angular_velocity = self.angular_velocity + domega * h
            theta = 0.5 * h * (self.angular_velocity + new_angular_velocity)
            dx = self.direction.x * math.cos(theta) - self.direction.y * math.sin(theta)
            dy = self.direction.x * math.sin(theta) + self.direction.y * math.cos(theta)
            new_direction = Vector(dx, dy)

            a = self.direction * (self.left_thrust + self.right_thrust) / self.mass
            new_velocity = self.velocity + a * h
            new_position = self.position + self.velocity * h + 0.5 * a * h * h

            self.position = new_position
            self.velocity = new_velocity
            self.direction = new_direction
            self.angular_velocity = new_angular_velocity

            t += h

def segment_line_intersect(a, n, p1, p2):
    q1 = a
    q2 = a+n
    denom = (q1.x-q2.x)*(p1.y-p2.y) - (q1.y-q2.y)*(p1.x-p2.x)
    if denom == 0: return None
    isect = Vector(
        ((q1.x*q2.y - q1.y*q2.x)*(p1.x-p2.x) - (q1.x-q2.x)*(p1.x*p2.y - p1.y*p2.x))/denom,
        ((q1.x*q2.y - q1.y*q2.x)*(p1.y-p2.y) - (q1.y-q2.y)*(p1.x*p2.y - p1.y*p2.x))/denom)

    x1 = p1.x
    x2 = p2.x
    y1 = p1.y
    y2 = p2.y
    if x1 > x2:
        x1, x2 = x2, x1
    if y1 > y2:
        y1, y2 = y2, y1

    eps = 0.0001
    if isect.x - x1 > -eps and isect.x - x2 < eps and isect.y - y1 > -eps and isect.y - y2 < eps:
        return isect
    return None

def intersect(p1, p2, polygons):
    """Figure out the distance along a line at which an intersection with 
    polygon occurs"""
    
    dist = float("inf")
    for p in polygons:
        for i in xrange(0, len(p)):
            q1 = p[i]
            q2 = p[(i+1)%len(p)]
            isect = segment_line_intersect(p1, p2-p1, q1, q2)
            if not isect: continue
            sgn = 1 if dot(isect - p1, p2 - p1) > 0 else -1
            if sgn < 0: continue
            dist = min(dist, norm(isect - p1))
    return dist

def arrow(window, (x1, y1), (x2, y2), color=(255, 255, 255), width=1):
    """Draw an arrow"""
    pygame.draw.line(window, color, (x1, y1), (x2, y2), width)

    arrow = pygame.Surface((8, 8), pygame.SRCALPHA, 32)
    pygame.draw.polygon(arrow, color, [(0, 0), (0, 7), (3, 3)])
    angle = math.degrees(math.atan2(-(y2-y1), x2-x1))

    trans = pygame.transform.rotate(arrow, angle)
    rect = trans.get_rect(center=(x2, y2))
    window.blit(trans, rect)

def update_sensor_data(ship, problem):
    """Cast rays from ship to find intersections with obstacles and pick out
    the best target point"""
    target_dir = normalized(problem.end_position - ship.position)
    max_score = 0
    ship.sensor_data = []

    def score(p):
        if norm(ship.position - problem.end_position) > norm(p - problem.end_position):
            return -1
        return 1/norm(problem.end_position - p) + norm(ship.position - p) + dot(ship.velocity, p - ship.position) + dot(ship.direction, p - ship.position)


    newpolys = problem.polygons[:]
    newpolys.append([problem.end_position + Vector(-ship.radius, -ship.radius),
                     problem.end_position + Vector(-ship.radius, ship.radius),
                     problem.end_position + Vector(ship.radius, ship.radius),
                     problem.end_position + Vector(ship.radius, -ship.radius)])
    for s in [1, -1]:
        for i in xrange(0, int(90/problem.sensor_degree)):
            theta = i*s*math.radians(problem.sensor_degree)
            ray_dir = Vector(
                target_dir.x * math.cos(theta) - target_dir.y * math.sin(theta),
                target_dir.x * math.sin(theta) + target_dir.y * math.cos(theta))
            dist = intersect(ship.position, ship.position+ray_dir, newpolys)
            p = ship.position + ray_dir*dist
            sc = score(p)
            if sc > max_score:
                max_score = sc
                ship.target = ship.position+dist*ray_dir
            if dist < float("inf"):
                ship.sensor_data.append(ship.position+dist*ray_dir)


def draw(window, problem, ship):
    """Draw the state of the world"""
    window.fill((0, 0, 0))

    x_min = min(problem.start_position.x - problem.radius,
            problem.end_position.x - problem.radius,
            *[p[0] for points in problem.polygons for p in points])
    x_max = max(problem.start_position.x + problem.radius,
            problem.end_position.x + problem.radius,
            *[p[0] for points in problem.polygons for p in points])
    y_min = min(problem.start_position.y - problem.radius,
            problem.end_position.y - problem.radius,
            *[p[1] for points in problem.polygons for p in points])
    y_max = max(problem.start_position.y + problem.radius,
            problem.end_position.y + problem.radius,
            *[p[1] for points in problem.polygons for p in points])

    x_min -= 1
    x_max += 1
    y_min -= 1
    y_max += 1

    width = x_max - x_min
    height = y_max - y_min
    
    if width > height:
        height = width * (float(WINDOW_HEIGHT) / WINDOW_WIDTH)
        y_min -= (height - width) / 2
    else:
        width = height * (float(WINDOW_WIDTH) / WINDOW_HEIGHT)
        x_min -= (width - height) / 2

    # point translation from world to screen coords
    p2s = lambda v: (int((v[0] - x_min) / width * WINDOW_WIDTH),
        WINDOW_HEIGHT - (int((v[1] - y_min) / height * WINDOW_HEIGHT)))
    # vector translation from world to screen coords
    v2s = lambda p: (int(p[0] / width * WINDOW_WIDTH),
        WINDOW_HEIGHT - (int(p[1] / height * WINDOW_HEIGHT)))    

    # Draw obstacles
    for poly in problem.polygons:
        pygame.draw.polygon(window, (255, 0, 0), [p2s(v) for v in poly])

    # Draw ship
    r = v2s((ship.radius, ship.radius))
    p = p2s(ship.position)
    pygame.draw.circle(window, (0, 0, 255), p, r[0], 1)

    # Draw direction arrow
    arrow_end = ship.position + ship.radius * ship.direction
    arrow(window, p, p2s(arrow_end), (0, 255, 0))

    # Draw thrust arrows
    dperp = Vector(-ship.direction.y, ship.direction.x)
    if ship.left_thrust:
        arrow_start = ship.position - ship.radius * dperp
        arrow_end = arrow_start - ship.left_thrust * ship.direction
        arrow(window, p2s(arrow_start), p2s(arrow_end), (255, 128, 0))
    if ship.right_thrust:
        arrow_start = ship.position + ship.radius * dperp
        arrow_end = arrow_start - ship.right_thrust * ship.direction
        arrow(window, p2s(arrow_start), p2s(arrow_end), (255, 128, 0))

    # Draw end position
    pygame.draw.circle(window, (0, 255, 255), p2s(problem.end_position), r[0], 0)

    # Draw sensor data
    #for p in ship.sensor_data:
    #    pygame.draw.circle(window, (255, 255, 0), p2s(p), 5, 0)

    # Draw target position
    if ship.target:
        pygame.draw.circle(window, (255, 0, 255), p2s(ship.target), 5, 0)

def main():
    """Main loop"""

    if len(sys.argv) < 2:
        print 'Usage: visualize.py <problem> [solution]'
        sys.exit(1)

    problem = Problem(sys.argv[1])
    ship = Ship()

    ship.position = problem.start_position
    ship.direction = problem.start_direction
    ship.radius = problem.radius
    ship.mass = problem.mass

    pygame.init()
    window = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
    fps = pygame.time.Clock()

    counter = 0

    # Message pump
    while True:
        for event in pygame.event.get():
            if event.type == QUIT:
                pygame.quit()
                sys.exit(0)
            elif event.type == KEYDOWN:
                if event.key == K_ESCAPE:
                    pygame.event.post(pygame.event.Event(QUIT))
        
        draw(window, problem, ship)

        pygame.display.update()
        elapsed = fps.tick(30) / 1000.0
        ship.step(elapsed)

        if counter % 10 == 0:
            update_sensor_data(ship, problem)
        counter += 1

        pressed = pygame.key.get_pressed()
        per_sec = 3.0 # Thrust change per second
        if pressed[K_q]: ship.left_thrust += elapsed * per_sec
        if pressed[K_a]: ship.left_thrust -= elapsed * per_sec
        if pressed[K_w]: ship.right_thrust += elapsed * per_sec
        if pressed[K_s]: ship.right_thrust -= elapsed * per_sec

if __name__ == '__main__':
    main()
