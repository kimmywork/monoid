-- ======= Chapter 4 =======

create keyspace if not exists store
    with replication = {
        'class': 'SimpleStrategy',
        'replication_factor': 1
        };

create table if not exists store.shopping_cart
(
    userid                text primary key,
    item_count            int,
    last_update_timestamp timestamp,
);

insert into store.shopping_cart
    (userid, item_count, last_update_timestamp)
VALUES ('9876', 2, toTimestamp(now()));

insert into store.shopping_cart
    (userid, item_count, last_update_timestamp)
VALUES ('1234', 5, toTimestamp(now()));

select *
from store.shopping_cart;

use store;

create table user
(
    first_name text,
    last_name  text,
    title      text,
    primary key ( last_name, first_name )
);

insert into user (first_name, last_name, title)
VALUES ('Bill', 'Nguyen', 'Mr.');

select *
from user;

select *
from user
where last_name = 'Nguyen';

select count(*)
from user;

delete title
from user
where first_name = 'Bill'
  and last_name = 'Nguyen';

select count(*)
from user;

alter table user
    add middle_initial text;

insert into user (first_name, last_name, title)
values ('Wanda', 'Nguyen', 'Mrs.');

insert into user (first_name, last_name)
values ('Mary', 'Rodriguez');

select *
from user
where last_name = 'Nguyen';

update store.user
set title = 'Mr.'
where first_name = 'Bill'
  and last_name = 'Nguyen';

insert into user (first_name, middle_initial, last_name, title)
values ('Bill', 'S', 'Nguyen', 'Mr.');
insert into user (first_name, middle_initial, last_name, title)
values ('Bill', 'R', 'Nguyen', 'Mr.');

select *
from user
where first_name = 'Bill'
  and last_name = 'Nguyen';

select first_name, last_name, title, writetime(title)
from user;

select first_name, middle_initial, last_name, ttl(middle_initial)
from user;

update user using ttl 3600
set middle_initial = 'Z'
where first_name = 'Mary'
  and last_name = 'Rodriguez';

insert into user (first_name, last_name)
values ('Jeff', 'Carpenter') using TTL 60;


select *
from user
where first_name = 'Jeff'
  and last_name = 'Carpenter';

alter table user
    add id uuid;

update user
set id = uuid()
where first_name = 'Mary'
  and last_name = 'Rodriguez';

create table user_visits
(
    user_id uuid primary key,
    visits  counter
);

update user_visits
set visits = visits + 1
where user_id = bb5ec77d-8a7e-44ed-8977-6d82b81ab300;

select *
from user_visits;

alter table user
    add emails set<text>;

update user
set emails = { 'mary@example.com' }
where first_name = 'Mary'
  and last_name = 'Rodriguez';

update user
set emails = emails + { 'mary.Rodriguez.AZ@gmail.com' }
where first_name = 'Mary'
  and last_name = 'Rodriguez';

select emails
from user;

alter table user
    add phone_numbers list<text>;

update user
set phone_numbers = ['1-800-999-9999']
where first_name = 'Mary'
  and last_name = 'Rodriguez';

select *
from user;

update user
set phone_numbers = phone_numbers + ['480-111-1111']
where first_name = 'Mary'
  and last_name = 'Rodriguez';

update user
set phone_numbers[1] = '480-111-1111'
where first_name = 'Mary'
  and last_name = 'Rodriguez';

delete phone_numbers[0]
from user
where first_name = 'Mary'
  and last_name = 'Rodriguez';

alter table user
    add login_sessions map<timeuuid, int>;

update user
set login_sessions = { now(): 13, now(): 18 }
where first_name = 'Mary'
  and last_name = 'Rodriguez';

select *
from user;

alter table user
    add address tuple<text, text, text, int>;

UPDATE user
SET address = ('7712 E. Broadway', 'Tucson', 'AZ', 85715)
WHERE first_name = 'Mary'
  AND last_name = 'Rodriguez';

select *
from user;

create type address (
    street text,
    city text,
    state text,
    zip_code int,
    );

-- alter table user add addresses map<text, address>;

alter table user
    add addresses map<text, frozen<address>>;

update user
set addresses = addresses + {
    'home': { street: '7112 E. Broadway', city: 'Tucson', state: 'AZ', zip_code: 85715 }
    }
where first_name = 'Mary'
  and last_name = 'Rodriguez';


-- ======= Chapter 5 =======


drop keyspace hotel;
drop keyspace reservation;


create keyspace hotel with replication = { 'class': 'SimpleStrategy', 'replication_factor': 3 };

create type hotel.address (
    street text,
    city text,
    state_or_province text,
    postal_code text,
    country text
    );

create table hotel.hotels_by_poi
(
    poi_name        text,
    poi_description text static,
    hotel_id        text,
    name            text,
    phone           text,
    address         frozen<address>,
    primary key ( (poi_name), hotel_id )
) with clustering order by (hotel_id asc);

create table hotel.hotels
(
    id      text primary key,
    name    text,
    phone   text,
    address frozen<address>,
    pois    set<text>,
);

create table hotel.pois_by_hotel
(
    poi_name    text,
    hotel_id    text,
    description text,
    primary key ( (hotel_id), poi_name)
);


create table hotel.available_rooms_by_hotel_date
(
    hotel_id     text,
    date         date,
    room_number  smallint,
    is_available boolean,
    primary key ( (hotel_id), date, room_number )
);



create table hotel.amenities_by_room
(
    hotel_id     text,
    room_number  smallint,
    amenity_name text,
    description  text,
    PRIMARY KEY ( (hotel_id, room_number), amenity_name )
);

create keyspace reservation with replication = {
    'class': 'SimpleStrategy',
    'replication_factor': 3
    };

use reservation;

create type reservation.address (
    street text, city text,
    state_or_province text, postal_code text,
    country text
    );

create table reservation.reservations_by_confirmation
(
    confirm_number text,
    hotel_id       text,
    start_date     date,
    end_date       date,
    room_number    smallint,
    guest_id       uuid,
    primary key ( confirm_number )
);

create table reservation.reservations_by_hotel_date
(
    hotel_id       text,
    start_date     date,
    room_number    smallint,
    end_date       date,
    confirm_number text,
    guest_id       uuid,

    primary key ( (hotel_id, start_date), room_number )
);

create table reservation.reservations_by_guest
(
    guest_last_name text,
    guest_id        uuid,
    confirm_number  text,
    hotel_id        text,
    start_date      date,
    end_date        date,
    room_number     smallint,

    primary key ( (guest_last_name), guest_id, confirm_number )
);

create table reservation.guests
(
    guest_id      uuid primary key,
    first_name    text,
    last_name     text,
    title         text,
    emails        set<text>,
    phone_numbers list<text>,
    addresses     map<text, frozen<address>>
);


drop table reservation.reservations_by_confirmation;

create materialized view reservation.reservations_by_confirmation
as select * from reservation.reservations_by_hotel_date
   where confirm_number is not null
     and hotel_id is not null
     and start_date is not null
     and room_number is not null
PRIMARY KEY ( confirm_number, hotel_id, start_date, room_number );

