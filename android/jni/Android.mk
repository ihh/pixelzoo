# Copyright (C) 2009 The Android Open Source Project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

MY_LOCAL_PATH := $(call my-dir)

# include $(call all-subdir-makefiles)
include $(MY_LOCAL_PATH)/xml2/Android.mk
include $(MY_LOCAL_PATH)/hello-jni/Android.mk

include $(MY_LOCAL_PATH)/Settings.mk # for SDL
include $(MY_LOCAL_PATH)/SDL/Android.mk

# include $(MY_LOCAL_PATH)/pixelzoo/Android.mk

LOCAL_PATH := $(MY_LOCAL_PATH)

include $(CLEAR_VARS)

# LOCAL_SRC_FILES := $(wildcard ../../src/*.c)
# LOCAL_SRC_FILES := $(wildcard ../../tsrc/*.c)

LOCAL_MODULE := androidhook
LOCAL_SRC_FILES := androidhook.c xmltest.c androidgame.c \
	../../src/move.c ../../src/xymap.c ../../src/board.c ../../src/xmlmove.c ../../src/color.c ../../src/rule.c \
	../../src/statemap.c ../../src/game.c ../../src/xmlutil.c ../../src/xmlgame.c ../../src/list.c ../../src/vector.c ../../src/quadtree.c \
	../../src/particle.c ../../src/util.c ../../src/notify.c ../../src/balloon.c ../../src/vars.c ../../src/tool.c ../../src/bintree.c \
	../../src/stringmap.c ../../src/xmlgoal.c ../../src/goal.c ../../src/mersenne.c ../../src/rbtree.c ../../src/xmlboard.c \
	../../src/optlist.c
	
LOCAL_C_INCLUDES := $(LOCAL_PATH)/xml2/include $(LOCAL_PATH)/SDL/include $(LOCAL_PATH)/../../src

LOCAL_LDLIBS := -llog
LOCAL_SHARED_LIBRARIES := xml2 SDL # pixelzoo
LOCAL_CFLAGS = -std=c99 -g

include $(BUILD_SHARED_LIBRARY)
