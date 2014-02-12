# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####

# Author: Adhi Hargo (cadmus.sw@gmail.com)

import bpy
import os
import re
import subprocess
import tempfile
from bpy.types import Operator, Menu, Panel
from bpy.props import BoolProperty, EnumProperty, IntProperty, StringProperty,\
    PointerProperty

bl_info = {
    "name": "ADH Fluid Concept",
    "author": "Adhi Hargo",
    "version": (1, 1, 0),
    "blender": (2, 69, 0),
    "location": "View3D > Tools > ADH Fluid Concept",
    "description": "In-house production tools.",
    "warning": "",
    "wiki_url": "https://github.com/adhihargo/fluid_concept",
    "tracker_url": "https://github.com/adhihargo/fluid_concept/issues",
    "category": "Sequencer"}

PRJ_IMG_PREFIX = 'PRJ_IMG_'
PRJ_UVMAP_PREFIX = 'PRJ_UVMAP_'
PRJ_MAT_PREFIX = 'PRJ_MAT_'
PRJ_TEX_PREFIX = 'PRJ_TEX_'
BASE_LAYER_SEPARATOR = '_'
HAS_CONVERT = any(filter(lambda p: os.path.exists(os.path.join(p, 'convert')),
                          os.environ.get('PATH', '').split(os.pathsep)))
HAS_XCFTOOLS = any(filter(lambda p: os.path.exists(os.path.join(p, 'xcf2png')),
                          os.environ.get('PATH', '').split(os.pathsep)))
XCFINFO_OUTPUT_RE = re.compile(r"""
(?P<visibility>[+-])\s
(?P<width>\d+)x(?P<height>\d+)\+(?P<offset_x>\d+)\+(?P<offset_y>\d+)\s
(?P<color_mode>\S+)\s
(?P<layer_mode>\S+)\s
(?P<layer_name>.+)$""", re.VERBOSE)

def edit_image_file(context, filepath, external_editor = True):
    image_editor = context.user_preferences.filepaths.image_editor
    if image_editor and external_editor:
        proc = subprocess.Popen([image_editor, filepath])
        return

    image = bpy.data.images.load(filepath)

    area_list = [a for a in context.screen.areas if a != context.area and
                 not a.type in ['SEQUENCE_EDITOR', 'INFO']]
    image_area = area_list[-1] if area_list else context.area
    image_area.type = 'IMAGE_EDITOR'
    image_space = image_area.spaces.active
    image_space.image = image
    image_space.mode = 'PAINT'

def get_temp_dir():
    temp_dir = bpy.context.user_preferences.filepaths.temporary_directory
    if not temp_dir:
        temp_dir = tempfile.gettempdir()

    return temp_dir

def get_xcf_layers(filepath):
    if not (HAS_XCFTOOLS and os.path.exists(filepath)):
        return []

    status, output_str = subprocess.getstatusoutput('xcfinfo "%s"' % filepath)
    if(status != 0):
        return []

    output = [m.group('layer_name')
              for m in map(lambda l: XCFINFO_OUTPUT_RE.match(l),
                           output_str.split('\n')[1:]) if m]
    return output

def get_master_file(filepath):
    filepath = bpy.path.abspath(filepath)
    dirname, filename = os.path.split(filepath)
    basename = os.path.splitext(filename)[0]
    if not filepath:
        return filepath

    master_filepath = os.path.join(dirname, basename + '.xcf')
    if not os.path.exists(master_filepath):
        # Assume filename is XCFNAME_LAYERNAME.EXT, try if XCFNAME.xcf matches
        rsep = basename.rfind(BASE_LAYER_SEPARATOR)
        while rsep != -1:
            master_filepath = os.path.join(dirname, basename[:rsep] + '.xcf')
            if os.path.exists(master_filepath):
                break
            rsep = basename.rfind(BASE_LAYER_SEPARATOR, 0, rsep)
    if not os.path.exists(master_filepath):
        master_filepath = filepath

    return master_filepath

def get_background_enums(self, context):
    space = context.space_data
    enums = [(str(i), b.image.name, 'Image "%s"' % b.image.name)
             for i, b in enumerate(space.background_images)
             if b.source == 'IMAGE']
    return enums

def get_scene_enums(self, context):
    enums = [(s.name, s.name, 'Scene "%s"' % s.name) for s in bpy.data.scenes]
    if not enums:
        enums.append((context.scene.name, "(no other scene)",
                      "There is no other scene to choose from."))
    return enums

def get_topmost_channel(sequences, frame_min, frame_max):
    frame_range = range(frame_min, frame_max)
    sequences_in_range = list(filter(lambda s:\
                                         s.frame_final_start in frame_range or\
                                         s.frame_final_end in frame_range,
                                     sequences))
    return max(map(lambda s: s.channel, sequences_in_range))\
        if sequences_in_range else 1

def get_uvmap_image(uvmap):
    image = None
    for uvmap_poly in uvmap.data:
        if uvmap_poly.image:
            image = uvmap_poly.image
            break

    return image

def get_fcurve(action, sequence):
    channel = 'volume' if sequence.type == 'SOUND' else 'blend_alpha'
    data_path = 'sequence_editor.sequences_all["%s"].%s' % (
        sequence.name, channel)

    fcurve = None
    for f in action.fcurves:
        if f.data_path == data_path:
            fcurve = f
            break
    if not fcurve:
        fcurve = action.fcurves.new(data_path)

    return fcurve

def render_image(context, filepath, scale = 100, scene = None, opengl = False):
    prev = {}
    # Save, render...
    if context.space_data.type == 'VIEW_3D':
        prev['view_persp'] = context.space_data.region_3d.view_perspective
    prev['scene'] = context.screen.scene
    if scene:
        context.screen.scene = scene

    render_settings = context.scene.render
    prev['filepath'] = render_settings.filepath
    prev['fileformat'] = render_settings.image_settings.file_format
    prev['render_ratio'] = render_settings.resolution_percentage
    prev['alpha_mode'] = render_settings.alpha_mode
    prev['color_mode'] = render_settings.image_settings.color_mode
    render_settings.filepath = filepath
    render_settings.image_settings.file_format = 'PNG'
    render_settings.resolution_percentage = scale
    render_settings.alpha_mode = 'TRANSPARENT'
    render_settings.image_settings.color_mode = 'RGBA'

    render_func = bpy.ops.render.opengl if opengl else bpy.ops.render.render
    render_func(animation = False, write_still = True)

    # ... restore.
    render_settings.filepath = prev['filepath']
    render_settings.image_settings.file_format = prev['fileformat']
    render_settings.resolution_percentage = prev['render_ratio']
    render_settings.alpha_mode = prev['alpha_mode']
    render_settings.image_settings.color_mode = prev['color_mode']

    context.screen.scene = prev['scene']
    if context.space_data.type == 'VIEW_3D':
        context.space_data.region_3d.view_perspective = prev['view_persp']

class OBJECT_OT_adh_copy_action(Operator):
    bl_idname = 'object.adh_copy_action'
    bl_label = 'Copy All Actions'
    bl_options = {'REGISTER', 'UNDO'}

    offset_frame = IntProperty(name = "Offset", subtype = "TIME")

    @classmethod
    def poll(self, context):
        return context.active_object

    def execute(self, context):
        obj = context.active_object
        other_objs = sorted(
            [o for o in context.selected_objects if o != obj],
            key = lambda o: ((o.location + o.delta_location) -
                             (obj.location + obj.delta_location)).length)
        actions = {}

        for owner, key in [(obj, "object"), (obj.data, "data")]:
            if owner.animation_data and owner.animation_data.action:
                actions[key] = owner.animation_data.action

        offset = self.offset_frame
        for o in other_objs:
            key = "location"
            delta = ((getattr(o, key) + getattr(o, "delta_"+key)) -
                     (getattr(obj, key) + getattr(obj, "delta_"+key)))
            setattr(o, "delta_"+key, delta)

            for owner, key in [(o, "object"), (o.data, "data")]:
                if not actions.get(key, None):
                    continue
                owner.animation_data_create()
                owner.animation_data.action = None

                action_name = actions[key].name + "_copy_" + o.name
                action = bpy.data.actions.get(action_name, None)
                if action:
                    bpy.data.actions.remove(action)
                action = actions[key].copy()
                action.name = action_name
                owner.animation_data.action = action

                fcurves = action.fcurves
                for curve in fcurves:
                    keyframePoints = curve.keyframe_points
                    for keyframe in keyframePoints:
                        keyframe.co[0] += offset
                        keyframe.handle_left[0] += offset
                        keyframe.handle_right[0] += offset

            offset += self.offset_frame

        return {'FINISHED'}

class MESH_OT_adh_project_background_image_to_mesh(Operator):
    bl_idname = 'mesh.adh_project_background_image_to_mesh'
    bl_label = 'Project Background Image to Mesh'
    bl_options = {'REGISTER', 'UNDO'}

    bkg_image_idx = EnumProperty(items = get_background_enums)

    invoked = False

    @classmethod
    def poll(self, context):
        return context.space_data.type == 'VIEW_3D'\
            and context.space_data.background_images\
            and context.object and context.object.type == 'MESH'

    def draw(self, context):
        layout = self.layout
        if self.invoked:
            return

        row = layout.row()
        row.label('Background Image:')
        row.prop(self, 'bkg_image_idx', text='')

    def execute(self, context):
        cam_list = [o for o in context.selected_objects if o.type == 'CAMERA']
        cam = cam_list[0] if cam_list else None

        obj = context.object
        scene = context.scene
        space = context.space_data
        image = space.background_images[int(self.bkg_image_idx)].image

        prev_perspective = space.region_3d.view_perspective
        prev_camera = scene.camera
        prev_mode = obj.mode
        space.region_3d.view_perspective = 'CAMERA'
        if cam:
            scene.camera = cam
        bpy.ops.object.mode_set(mode = 'EDIT')

        bpy.ops.mesh.select_all(action = 'SELECT')
        bpy.ops.uv.project_from_view(
            orthographic = (scene.camera.type == 'ORTHO'),
            camera_bounds = True, correct_aspect = False,
            clip_to_bounds = False, scale_to_bounds = False)

        bpy.ops.object.mode_set(mode=prev_mode)
        space.region_3d.view_perspective = prev_perspective
        scene.camera = prev_camera

        uvmap = obj.data.uv_textures.active
        uvmap.name = PRJ_UVMAP_PREFIX + scene.camera.name
        for uvmap_poly in uvmap.data:
            uvmap_poly.image = image

        mat_name = PRJ_MAT_PREFIX + obj.name
        mat = bpy.data.materials.get(mat_name, None)
        if not mat:
            mat = bpy.data.materials.new(mat_name)
        mat.game_settings.alpha_blend = 'ALPHA'
        mat.use_shadeless = True

        tex_name = PRJ_TEX_PREFIX + obj.name
        tex = bpy.data.textures.get(tex_name, None)
        if not tex:
            tex = bpy.data.textures.new(tex_name, 'IMAGE')
        tex.image = image

        texslot = mat.texture_slots.create(0)
        texslot.texture_coords = 'UV'
        texslot.uv_layer = uvmap.name
        texslot.texture = tex

        obj.data.materials.clear()
        obj.data.materials.append(mat)

        return {'FINISHED'}

    def invoke(self, context, event):
        retval = context.window_manager.invoke_props_dialog(self)
        self.invoked = True
        return retval

class VIEW3D_OT_adh_background_image_from_scene(Operator):
    bl_idname = 'view3d.adh_background_image_from_scene'
    bl_label = 'Add Background Image from Scene'
    bl_options = {'REGISTER', 'UNDO'}

    scale = IntProperty(min = 0, max = 100,
                        default = 100, subtype = 'PERCENTAGE')
    scene_name = EnumProperty(items = get_scene_enums)
    dirpath = StringProperty(subtype = 'DIR_PATH', default = "//")
    external_editor = BoolProperty(default = True)
    opengl = BoolProperty(default = True)
    invoked = False

    @classmethod
    def poll(self, context):
        return context.space_data.type == 'VIEW_3D'

    def draw(self, context):
        layout = self.layout
        if self.invoked:
            return
        
        row = layout.row()
        row.label("Scene:") ; row.prop(self, 'scene_name', text="")
        row = layout.row()
        row.label("Resolution Percentage:") ; row.prop(self, 'scale', text="")
        row = layout.row()
        row.label("File Location:") ; row.prop(self, 'dirpath', text="")
        row = layout.row()
        row.label("OpenGL:") ; row.prop(self, 'opengl', text=" ")

    def execute(self, context):
        space = context.space_data
        scene = bpy.data.scenes[self.scene_name]
        if not scene.camera:
            self.report({'ERROR'},
                        'Scene "%s" has no active camera.' % self.scene_name)
            return {'CANCELLED'}

        bkg_preview = None
        prv_filename = PRJ_IMG_PREFIX + self.scene_name + '.png'
        prv_filepath = os.path.join(self.dirpath, prv_filename)
        for bkg_image in space.background_images:
            if bkg_image.image and bkg_image.image.filepath == prv_filepath:
                bkg_preview = bkg_image
                continue
            space.background_images.remove(bkg_image)

        render_image(context, prv_filepath, scale = self.scale,
                     scene = scene, opengl = self.opengl)

        if not bkg_preview:
            bkg_preview = space.background_images.new()
            bkg_image = bpy.data.images.load(prv_filepath)
            bkg_image.name = prv_filename
            bkg_preview.image = bkg_image
        else:
            bkg_preview.image.filepath = prv_filepath
            bkg_preview.image.reload()
        bkg_preview.show_expanded = False
        bkg_preview.view_axis = 'CAMERA'

        space.show_background_images = True
        return {'FINISHED'}

    def invoke(self, context, event):
        retval = context.window_manager.invoke_props_dialog(self)
        self.invoked = True
        return retval

class SEQUENCER_OT_adh_add_annotation_image_strip(Operator):
    bl_idname = 'sequencer.adh_add_annotation_image_strip'
    bl_label = 'Add Annotation Image Strip'
    bl_options = {'REGISTER', 'UNDO'}

    scene_name = EnumProperty(items = get_scene_enums)
    filepath = StringProperty(subtype = 'FILE_PATH', default = "//frame.png")
    external_editor = BoolProperty(default = True)
    opengl = BoolProperty(default = True)
    invoked = False

    @classmethod
    def poll(self, context):
        return context.space_data.type == 'SEQUENCE_EDITOR'

    def draw(self, context):
        layout = self.layout
        if self.invoked:
            return

        row = layout.row()
        row.label("Scene:") ; row.prop(self, 'scene_name', text="")
        row = layout.row()
        row.label('File Path:') ; row.prop(self, 'filepath', text='')
        row = layout.row()
        row.label("OpenGL:") ; row.prop(self, 'opengl', text=" ")
        row = layout.row()
        row.label('Use External Editor:')
        row.prop(self, 'external_editor', text=' ')

    def execute(self, context):
        self.filepath = bpy.path.abspath(self.filepath)

        render_scene = bpy.data.scenes[self.scene_name]
        render_image(context, self.filepath, scene = render_scene,
                     opengl = self.opengl)
        if not os.path.exists(self.filepath):
            self.report({'ERROR'}, "Render output file doesn't exist.")
            return {'CANCELLED'}

        scene = context.scene
        strip_duration = 50
        topmost_channel = 1
        scene.sequence_editor_create()
        sequences = scene.sequence_editor.sequences
        if scene.sequence_editor:
            frame_current = scene.frame_current
            topmost_channel = get_topmost_channel(
                sequences, frame_current, frame_current + strip_duration)
        image_strip = sequences.new_image(
            os.path.basename(self.filepath), self.filepath,
            topmost_channel + 1, scene.frame_current)
        image_strip.blend_type = 'ALPHA_OVER'
        image_strip.frame_final_duration = strip_duration
        image_strip.mute = True # Unmute manually after edit = reload image

        edit_image_file(context, self.filepath, self.external_editor)
        
        return {'FINISHED'}

    def invoke(self, context, event):
        props = context.scene.adh_fluid_concept
        if props.new_strip_image_filepath:
            self.filepath = props.new_strip_image_filepath
        else:            
            self.filepath = '//frame_%04d.png' % (context.scene.frame_current)

        retval = context.window_manager.invoke_props_dialog(self)
        self.invoked = True
        return retval

class SEQUENCER_OT_adh_fade_in_out_selected_strips(Operator):
    bl_idname = 'sequencer.adh_fade_in_out_selected_strips'
    bl_label = 'Fade In/Out Selected Strips'
    bl_options = {'REGISTER', 'UNDO'}

    do_fade_in = BoolProperty(default = True)
    do_fade_out = BoolProperty(default = True)
    fade_in_secs = IntProperty(default = 2, min = 0)
    fade_out_secs = IntProperty(default = 2, min = 0)

    invoked = False

    @classmethod
    def poll(self, context):
        return context.selected_sequences

    def draw(self, context):
        layout = self.layout
        if self.invoked:
            return

        row = layout.row()
        row.prop(self, 'do_fade_in', text = '')
        row.label('Fade In:')
        row.prop(self, 'fade_in_secs', text = '')

        row = layout.row()
        row.prop(self, 'do_fade_out', text = '')
        row.label('Fade Out:')
        row.prop(self, 'fade_out_secs', text = '')
    
    def execute(self, context):
        scene = context.scene
        if not scene.animation_data:
            scene.animation_data_create()

        action = scene.animation_data.action
        if not action:
            action = bpy.data.actions.new(scene.name + "_sequence_anim")
            scene.animation_data.action = action

        fps = context.scene.render.fps
        for sequence in context.selected_sequences:
            fcurve = get_fcurve(action, sequence)

            frame_duration = sequence.frame_final_duration
            fade_in_frames = self.fade_in_secs * fps
            fade_out_frames = self.fade_out_secs * fps
            frame_shortage = -frame_duration + (
                (fade_in_frames if self.do_fade_in else 0) +
                (fade_out_frames if self.do_fade_out else 0))                
            if frame_shortage > 0:
                fade_in_frames -= frame_shortage / 2
                fade_out_frames -= frame_shortage / 2

            frame_start = sequence.frame_final_start
            frame_end =  sequence.frame_final_end
            
            for point_x in [p.co.x for p in fcurve.keyframe_points if
                            p.co.x > frame_start and p.co.x < frame_end]:
                point = next(filter(lambda p: p.co.x == point_x,
                                    fcurve.keyframe_points), None)
                fcurve.keyframe_points.remove(point)
            for frame, value in [(frame_start, 0 if self.do_fade_in else 1),
                                 (frame_start + fade_in_frames, 1),
                                 (frame_end - fade_out_frames, 1),
                                 (frame_end, 0 if self.do_fade_out else 1)]:
                point = fcurve.keyframe_points.insert(frame, value)
                point.interpolation = 'LINEAR'

        return {'FINISHED'}

    def invoke(self, context, event):
        retval = context.window_manager.invoke_props_dialog(self)
        self.invoked = True
        return retval

class ImageMixin:
    image = None
    filepath = None
    master_filepath = None

    def init_variables(self, context):
        scene = context.scene
        space = context.space_data
        obj = context.active_object
        seq = context.selected_editable_sequences[0]\
            if context.selected_editable_sequences else None

        if space.type == "IMAGE_EDITOR" and space.image:
            self.image = space.image
            self.filepath = bpy.path.abspath(space.image.filepath)
        elif obj and obj.type == 'MESH' and obj.data.uv_textures.active:
            self.image = get_uvmap_image(obj.data.uv_textures.active)
            if self.image:
                self.filepath = bpy.path.abspath(self.image.filepath)
        elif seq and seq.type == 'IMAGE':
            seq_element = seq.strip_elem_from_frame(scene.frame_current)
            filedir = bpy.path.abspath(seq.directory)
            filename = seq_element.filename
            self.filepath = os.path.join(filedir, filename)

        if self.filepath:
            self.master_filepath = get_master_file(self.filepath)

class IMAGE_OT_adh_external_edit_master(Operator, ImageMixin):
    bl_idname = 'image.adh_external_edit_master'
    bl_label = 'Master Image Edit Externally'
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        self.init_variables(context)
        if not self.filepath:
            self.report({"WARNING"}, "No image file within context.")
            return {'CANCELLED'}

        edit_image_file(context, self.master_filepath)
        return {'FINISHED'}

class IMAGE_OT_adh_reload_from_master_file(Operator, ImageMixin):
    bl_idname = 'image.adh_reload_from_master_file'
    bl_label = 'Reload From Master File'
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        if not HAS_XCFTOOLS:
            self.report({"ERROR"}, "Needs 'xcf2png' from xcftools package")
            return {'CANCELLED'}

        self.init_variables(context)
        if not self.filepath:
            self.report({"WARNING"}, "No image file within context.")
            return {'CANCELLED'}

        if self.master_filepath.lower().endswith(".xcf"):
            status, output_str = subprocess.getstatusoutput(
                'xcf2png "%s" -o "%s"' % (self.master_filepath, self.filepath))
            if status != 0:
                self.report({"WARNING"}, output_str.replace("\n", " "))

        if self.image:
            self.image.reload()
        context.area.tag_redraw()
        return {'FINISHED'}

class IMAGE_OT_adh_create_scaled_copy(Operator, ImageMixin):
    bl_idname = 'image.adh_create_scaled_copy'
    bl_label = 'Create Scaled Down Image Copy'
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        if not HAS_CONVERT:
            self.report({"ERROR"}, "Needs 'convert' from ImageMagick package")
            return {'CANCELLED'}

        self.init_variables(context)
        if not self.image:
            self.report({"WARNING"}, "No image data within context.")
            return {'CANCELLED'}

        # Infer the original and scaled-down files' name.
        dirname, ori_filename = os.path.split(self.filepath)
        basename = os.path.splitext(ori_filename)[0]
        scl_filename = ori_filename
        if basename.endswith("_025"):
            self.filepath, scl_filepath = self.filepath[:-4], self.filepath
        else:
            scl_filename = basename + "_025.png"
            scl_filepath = os.path.join(dirname, scl_filename)

        # Get the master file, and reload from it if exists.
        if self.master_filepath.lower().endswith(".xcf"):
            status, output_str = subprocess.getstatusoutput(
                'xcf2png "%s" -o "%s"' % (self.master_filepath, self.filepath))
            if status != 0:
                self.report({"WARNING"}, output_str.replace("\n", " "))

        # Create the scaled-down image.
        status, output_str = subprocess.getstatusoutput(
            'convert "%s" -resize "25%%" "%s"' % (self.filepath, scl_filepath))
        if status != 0:
            self.report({"ERROR"}, output_str.replace("\n", " "))
            return {"CANCELLED"}

        # Load the scaled-down image in Blender, replace the original with it.
        scl_image = bpy.data.images.get(scl_filename, None)
        if not scl_image:
            scl_image = self.image.copy()
            scl_image.name = scl_filename
        scl_image.filepath = scl_filepath

        return {'FINISHED'}

NODE_INCREMENT_X = 50
NODE_INCREMENT_Y = 50
RL_PASS_IMAGE = [('combined', 'Image'), ('mist', 'Mist'), ('color', 'Color'),
                 ('diffuse', 'Diffuse'),
                 ('specular', 'Specular'), ('shadow', 'Shadow'),
                 ('emit', 'Emit'), ('ambient_occlusion', 'AO'),
                 ('environment', 'Environment'), ('indirect', 'Indirect'),
                 ('reflection', 'Reflect'), ('refraction', 'Refract')]
RL_PASS_DATA = [('z', 'Z'), ('vector', 'Speed'), ('normal', 'Normal'),
                ('object_index', 'IndexOB'), ('material_index', 'IndexMA')]
RL_NODE_PREFIX = "RL "
FO_NODE_PREFIX = "FOI "
FOD_NODE_PREFIX = "FOD "
FI_NODE_PREFIX = "FI "
RL_NODE_TYPE = "CompositorNodeRLayers"
FO_NODE_TYPE = "CompositorNodeOutputFile"
FI_NODE_TYPE = "CompositorNodeImage"

def remove_unconnected_node_inputs(links, node):
    incoming_links = [l.to_socket for l in links
                      if l.to_node == node and l.from_socket.enabled]
    for i in filter(lambda i: not i in incoming_links, node.inputs):
        node.inputs.remove(i)

def create_node(scene, key, node_type):
    # Create a new node in SCENE's node tree of NODE_TYPE type, or
    # reuse one for which KEY predicate returns True.
    scene.use_nodes = True
    tree = scene.node_tree

    possible_nodes = [
        n for n in tree.nodes if n.bl_idname == node_type and key(n)]
    node = possible_nodes[0] if possible_nodes else\
        tree.nodes.new(type = node_type)
    node.show_preview = False
    node.select = False

    return node

def create_fo_node(scene, prefix):
    props = scene.adh_fluid_concept
    tree = scene.node_tree

    node_name = prefix + scene.name
    node = create_node(
        scene, lambda n: n.name.startswith(node_name), FO_NODE_TYPE)
    node.location = (-(node.width + NODE_INCREMENT_X), 0)
    node.name = node_name
    node.base_path = props.renderpass_basepath
    node.format.color_mode = 'RGBA'

    remove_unconnected_node_inputs(tree.links, node)
    return node

def create_fi_node(scene, basepath, dirname, filename):
    f_abspath = os.path.join(basepath, dirname, filename)
    possible_images = [i for i in bpy.data.images if i.filepath == f_abspath]
    image = possible_images[0] if possible_images\
        else bpy.data.images.load(f_abspath)

    node_name = FI_NODE_PREFIX + "_".join(
        [bpy.path.basename(basepath), dirname])
    node = create_node(
        scene, lambda n: n.name.startswith(node_name), FI_NODE_TYPE)
    node.image = image
    node.name = node_name
    node.label = dirname
    node.hide = True
    node.width_hidden = 100.0

    return node

class NODE_OT_adh_save_render_passes(Operator):
    bl_idname = "node.adh_save_render_passes"
    bl_label = "Save Render Passes"
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        scene = context.scene
        props = scene.adh_fluid_concept
        scene.use_nodes = True
        tree = scene.node_tree

        fo_node = create_fo_node(scene, FO_NODE_PREFIX)
        fod_node = create_fo_node(scene, FOD_NODE_PREFIX)
        fod_node.location[1] -= (fo_node.dimensions[1] + NODE_INCREMENT_Y)
        fod_node.format.file_format = "OPEN_EXR"
        
        node_loc = [0, 0]
        for rl in scene.render.layers:
            rl_node = create_node(
                scene, lambda n: n.layer == rl.name, RL_NODE_TYPE)
            rl_node.layer = rl.name
            if not rl_node.parent:
                rl_node.location = node_loc
            node_loc[1] -= (rl_node.dimensions[1] + NODE_INCREMENT_Y)
            rl_node.name = RL_NODE_PREFIX + rl.name
            rl_node.label = rl_node.name

            def link_pass_to_file(node, pass_name, output_name):
                rl_pass = getattr(rl, "use_pass_" + pass_name, False)
                if not rl_pass:
                    return
            
                output_path = os.sep.join(
                    [rl.name+'_'+pass_name, pass_name+'_'])
                rl_output = rl_node.outputs.get(output_name)
                fo_input = node.inputs.get(output_path, None)
                if fo_input:
                    return

                fo_slot = node.file_slots.new(output_path)
                fo_input = node.inputs.get(output_path, None)
                tree.links.new(rl_output, fo_input)

            # Connect passes
            for (p, o) in RL_PASS_IMAGE:
                link_pass_to_file(fo_node, p, o)
            for (p, o) in RL_PASS_DATA:
                link_pass_to_file(fod_node, p, o)

        return {'FINISHED'}

class NODE_OT_adh_load_render_passes(Operator):
    bl_idname = "node.adh_load_render_passes"
    bl_label = "Load Render Passes"
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        scene = context.scene
        props = scene.adh_fluid_concept
        scene.use_nodes = True
        tree = scene.node_tree

        # To make life easier, and the code faster, files in the
        # folders below BASEPATH is assumed to be image files ONLY,
        # and sequential. As it should be if we're well-organized.
        node_loc = [0, 0]
        basepath = bpy.path.abspath(props.renderpass_basepath)
        for d in sorted([d for d in os.listdir(basepath)
                         if os.path.isdir(os.path.join(basepath, d))]):
            d_abspath = os.path.join(basepath, d)
            files = sorted([f for f in os.listdir(d_abspath)
                            if os.path.isfile(os.path.join(d_abspath, f))])
            file_count = len(files)
            if file_count == 0:
                continue

            fi_node = create_fi_node(scene, basepath, d, files[0])
            if not fi_node.parent:
                fi_node.location = node_loc
            node_loc[1] -= (fi_node.dimensions[1] + NODE_INCREMENT_Y)
            if file_count > 1:
                fi_node.image.source = 'SEQUENCE'
                fi_node.frame_duration = file_count
            else:
                fi_node.image.source = 'FILE'

        return {'FINISHED'}    

class VIEW3D_PT_fluid_concept(Panel):
    bl_label = "ADH Fluid Concept"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"

    def draw(self, context):
        layout = self.layout

        col = layout.column(align = True)
        col.label("Background Images:")
        col.operator('view3d.adh_background_image_from_scene',
                     text = "Add From Scene")
        col.operator('mesh.adh_project_background_image_to_mesh',
                     text = "Project to Mesh")
        
        col = layout.column(align = True)
        col.label("Mesh Image:")
        col.operator("image.adh_external_edit_master",
                     text = "Edit Externally")
        col.operator("image.adh_reload_from_master_file",
                     text = "Reload From Master")
        col.operator('image.adh_create_scaled_copy',
                     text = "Create Scaled Copy")

class IMAGE_PT_fluid_concept(Panel):
    bl_label = "ADH Fluid Concept"
    bl_space_type = "IMAGE_EDITOR"
    bl_region_type = "UI"

    def draw(self, context):
        layout = self.layout

        col = layout.column(align = True)
        col.operator("image.adh_external_edit_master",
                     text = "Edit Externally")
        col.operator("image.adh_reload_from_master_file",
                     text = "Reload From Master")
        col.operator('image.adh_create_scaled_copy',
                     text = "Create Scaled Copy")

class SEQUENCER_PT_fluid_concept(Panel):
    bl_label = "ADH Fluid Concept"
    bl_space_type = "SEQUENCE_EDITOR"
    bl_region_type = "UI"

    def draw(self, context):
        layout = self.layout
        props = context.scene.adh_fluid_concept

        layout.prop(props, "new_strip_image_filepath", text = "")
        layout.operator("sequencer.adh_add_annotation_image_strip")

        col = layout.column(align = True)
        col.operator("image.adh_external_edit_master",
                     text = "Edit Externally")
        col.operator("image.adh_reload_from_master_file",
                     text = "Reload From Master")

        col = layout.column(align = True)
        col.operator("sequencer.adh_fade_in_out_selected_strips")

class NODE_PT_fluid_concept(Panel):
    bl_label = "ADH Fluid Concept"
    bl_space_type = "NODE_EDITOR"
    bl_region_type = "TOOLS"

    def draw(self, context):
        layout = self.layout
        space = context.space_data
        props = context.scene.adh_fluid_concept

        if space.tree_type == 'CompositorNodeTree':
            col = layout.column(align = True)
            col.prop(props, "renderpass_basepath", text = "")
            col.operator("node.adh_save_render_passes")
            col.operator("node.adh_load_render_passes")

class ADH_FluidConceptProps(bpy.types.PropertyGroup):
    new_strip_image_filepath = StringProperty(
        name = "Image Filepath",
        subtype = "FILE_PATH")

    renderpass_basepath = StringProperty(
        name = "Render Passes Base Path",
        subtype = "DIR_PATH",
        default = "//")

def draw_view3d_background_panel(self, context):
    layout = self.layout

    col = layout.column(align = True)
    col.operator('view3d.adh_background_image_from_scene',
                 text = "Add From Scene")
    col.operator('mesh.adh_project_background_image_to_mesh',
                 text = "Project to Mesh")

def draw_sequencer_add_strip_menu(self, context):
    layout = self.layout

    layout.operator_context = 'INVOKE_DEFAULT' # It's changed to EXEC somewhere
    layout.operator('sequencer.adh_add_annotation_image_strip',
                    text = "Annotation Image")

def register():
    bpy.types.VIEW3D_PT_background_image.prepend(draw_view3d_background_panel)
    bpy.types.SEQUENCER_MT_add.append(draw_sequencer_add_strip_menu)
    bpy.utils.register_module(__name__)

    bpy.types.Scene.adh_fluid_concept = PointerProperty\
        (type = ADH_FluidConceptProps)

def unregister():
    bpy.types.VIEW3D_PT_background_image.remove(draw_view3d_background_panel)
    bpy.types.SEQUENCER_MT_add.remove(draw_sequencer_add_strip_menu)
    bpy.utils.unregister_module(__name__)

    del bpy.types.Scene.adh_fluid_concept

if __name__ == "__main__":
    register()
