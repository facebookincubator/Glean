enum fb303_status {
  DEAD = 0,
  STARTING = 1,
  ALIVE = 2,
  STOPPING = 3,
  STOPPED = 4,
  WARNING = 5,
}

service BaseService {
  fb303_status getStatus() (priority = 'IMPORTANT');
  string getName();
  i64 aliveSince() (priority = 'IMPORTANT');
}
