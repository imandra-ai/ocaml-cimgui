open Ctypes

module Types = Imgui_opengl3_types_f.Make(Imgui_opengl3_generated_types)

module Make(F : Cstubs.FOREIGN) = struct
  open! Types
  open! F

  (** @param glsl_version = NULL *)
  let init =
    foreign "ImGui_ImplOpenGL3_Init"
      (string_opt @-> returning bool)

  let shutdown =
    foreign "ImGui_ImplOpenGL3_Shutdown"
      (void @-> returning void)

  let new_frame =
    foreign "ImGui_ImplOpenGL3_NewFrame"
      (void @-> returning void)

  let render_data =
    foreign "ImGui_ImplOpenGL3_RenderDrawData"
      (ptr Imgui_sys.ImDrawData.t @-> returning void)

  (* TODO

// (Optional) Called by Init/NewFrame/Shutdown
IMGUI_IMPL_API bool     ImGui_ImplOpenGL3_CreateFontsTexture();
IMGUI_IMPL_API void     ImGui_ImplOpenGL3_DestroyFontsTexture();
IMGUI_IMPL_API bool     ImGui_ImplOpenGL3_CreateDeviceObjects();
IMGUI_IMPL_API void     ImGui_ImplOpenGL3_DestroyDeviceObjects();
     *)
end
