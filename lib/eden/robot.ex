defmodule Eden.Robot do
  use Hedwig.Robot, otp_app: :eden

  def after_connect(state) do
    Hedwig.Robot.register(self, __MODULE__)
    {:ok, state}
  end

end
