{
  username,
  ...
}:

{
  networking.hostName = "no-mans-land";

  users.users.${username}.uid = 502;
}
