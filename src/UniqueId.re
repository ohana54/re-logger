let id = ref(0);

let make = () => {
  id := id^ + 1;
  id^;
};
