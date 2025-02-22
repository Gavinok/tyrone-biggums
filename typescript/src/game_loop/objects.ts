import { AABB, Collidable } from "./geometry";
import { Moveable, scale, Vector2D, Velocity } from "./physics";
import AttachablePool, { Attachable } from "./pool";

const playerPool = new AttachablePool<null, Player>(1200, () => new Player([0, 0], [0, 0], 0));

export class Player implements Collidable<AABB>, Attachable<null> {
    public geo: AABB;
    private lastFire: number;

    constructor(pos: Vector2D, public dir: Vector2D, private fireRate: number) {
        this.lastFire = 0;
        this.geo = AABB.fromWidthHeight(100, 100).setPosition(pos) as AABB; // this Geometry<AABB> sucks..
    }

    attach() {}
    detach() {}

    fire(): boolean {
        const now = Date.now();
        if (this.fireRate > now - this.lastFire) {
            return false;
        }

        this.lastFire = now;
        return true;
    }

    static create(posX: number, posY: number, dirX: number, fireRate: number): Player {
        const player = playerPool.pop();

        player.geo.setPositionXY(posX, posY);
        player.dir[0] = dirX;
        player.fireRate = fireRate;

        return player;
    }

    static release(player: Player): void {
        playerPool.push(player);
    }
}

const bulletPool = new AttachablePool<null, Bullet>(4000, () => new Bullet([0, 0], [0, 0]));
export class Bullet implements Collidable<AABB>, Moveable, Velocity {
    public geo: AABB;

    get pos(): Vector2D {
        // @ts-ignore no its not possibly undefined.  The constructor ensures
        // that it does exist
        return this.geo.pos;
    }

    constructor(pos: Vector2D, public vel: Vector2D, geo?: AABB) {
        this.geo = geo || Bullet.standardBulletGeometry(pos);
    }

    applyDelta(delta: Vector2D): void {
        this.geo.applyDelta(delta);
    }

    cleanUp(): void {
        bulletPool.push(this);
    }

    attach() {}
    detach(): void {}

    static standardBulletGeometry(pos: Vector2D): AABB {
        const aabb = AABB.fromWidthHeight(Bullet.BulletWidth, 3);
        aabb.setPosition(pos);

        return aabb;
    }

    static createFromPlayer(player: Player, speed: number): Bullet {
        const bullet = bulletPool.pop();

        if (player.dir[0] === 1) {
            bullet.geo.setPositionXY(
                player.geo.pos[0] + player.geo.width + 1,
                0);
        } else {
            bullet.geo.setPositionXY(
                player.geo.pos[0] - Bullet.BulletWidth - 1,
                0);
        }

        bullet.vel[0] = player.dir[0] * speed;
        bullet.vel[1] = player.dir[1] * speed;

        return bullet;
    }

    static BulletWidth = 35;
}
