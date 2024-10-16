var jsPsychVideoKeyboardResponse = (function(jspsych) {
    "use strict";

    const info = {
        name: "video-keyboard-response",
        parameters: {
            /** Array of the video file(s) to play. Video can be provided in multiple file formats for better cross-browser support. */
            stimulus: {
                type: jspsych.ParameterType.VIDEO,
                pretty_name: "Video",
                default: undefined,
                array: true,
            },
            /** Array containing the key(s) the subject is allowed to press to respond to the stimulus. */
            choices: {
                type: jspsych.ParameterType.KEYS,
                pretty_name: "Choices",
                default: "ALL_KEYS",
            },
            /** Any content here will be displayed below the stimulus. */
            prompt: {
                type: jspsych.ParameterType.HTML_STRING,
                pretty_name: "Prompt",
                default: null,
            },
            /** The width of the video in pixels. */
            width: {
                type: jspsych.ParameterType.INT,
                pretty_name: "Width",
                default: "",
            },
            /** The height of the video display in pixels. */
            height: {
                type: jspsych.ParameterType.INT,
                pretty_name: "Height",
                default: "",
            },
            /** If true, the video will begin playing as soon as it has loaded. */
            autoplay: {
                type: jspsych.ParameterType.BOOL,
                pretty_name: "Autoplay",
                default: true,
            },
            canvas_colour: {
                type: jspsych.ParameterType.STRING,
                array: false,
                pretty_name: "Colour",
                default: "rgba(100, 100, 100, 0.5)",
                description: "Canvas colour.",
            },
            canvas_size: {
                type: jspsych.ParameterType.Array,
                pretty_name: "Size",
                default: [1280, 960],
                description: "Canvas size.",
            },
            canvas_border: {
                type: jspsych.ParameterType.STRING,
                pretty_name: "Border",
                default: "10px solid black",
                description: "Border style",
            },
            translate_origin: {
                type: jspsych.ParameterType.BOOL,
                pretty_name: "Translate",
                default: false,
                description: "Translate origin to center",
            },

            /** If true, the subject will be able to pause the video or move the playback to any point in the video. */
            controls: {
                type: jspsych.ParameterType.BOOL,
                pretty_name: "Controls",
                default: false,
            },
            /** Time to start the clip. If null (default), video will start at the beginning of the file. */
            start: {
                type: jspsych.ParameterType.FLOAT,
                pretty_name: "Start",
                default: null,
            },
            /** Time to stop the clip. If null (default), video will stop at the end of the file. */
            stop: {
                type: jspsych.ParameterType.FLOAT,
                pretty_name: "Stop",
                default: null,
            },
            /** The playback rate of the video. 1 is normal, <1 is slower, >1 is faster. */
            rate: {
                type: jspsych.ParameterType.FLOAT,
                pretty_name: "Rate",
                default: 1,
            },
            /** If true, the trial will end immediately after the video finishes playing. */
            trial_ends_after_video: {
                type: jspsych.ParameterType.BOOL,
                pretty_name: "End trial after video finishes",
                default: false,
            },
            /** How long to show trial before it ends. */
            trial_duration: {
                type: jspsych.ParameterType.INT,
                pretty_name: "Trial duration",
                default: null,
            },
            /** If true, the trial will end when subject makes a response. */
            response_ends_trial: {
                type: jspsych.ParameterType.BOOL,
                pretty_name: "Response ends trial",
                default: true,
            },
            /** If true, then responses are allowed while the video is playing. If false, then the video must finish playing before a response is accepted. */
            response_allowed_while_playing: {
                type: jspsych.ParameterType.BOOL,
                pretty_name: "Response allowed while playing",
                default: true,
            },
            answer_font: {
                type: jspsych.ParameterType.STRING,
                pretty_name: "Font",
                default: "bold 50px Arial",
            },
            answer_colour: {
                type: jspsych.ParameterType.STRING,
                pretty_name: "Colour",
                default: "Black",
            },
            answer_text: {
                type: jspsych.ParameterType.STRING,
                pretty_name: "Text",
                default: "Antwort?",
            },
            answer_position: {
                type: jspsych.ParameterType.Array,
                pretty_name: "Position",
                default: [0, -175],
            },
            box_colour_left: {
                type: jspsych.ParameterType.STRING,
                pretty_name: "Box colour left",
                default: "Black",
            },
            box_colour_right: {
                type: jspsych.ParameterType.STRING,
                pretty_name: "Box colour right",
                default: "Black",
            },
            box_position: {
                type: jspsych.ParameterType.Array,
                pretty_name: "Box position",
                default: [100, 100],
            },
            box_size: {
                type: jspsych.ParameterType.INT,
                pretty_name: "Box position",
                default: 100,
            },
            box_frame: {
                type: jspsych.ParameterType.INT,
                pretty_name: "Box frame",
                default: 5,
            },
        },
    };

    /**
     * **video-keyboard-response**
     *
     * jsPsych plugin for playing a video file and getting a keyboard response
     *
     * @author Josh de Leeuw
     * @see {@link https://www.jspsych.org/plugins/jspsych-video-keyboard-response/ video-keyboard-response plugin documentation on jspsych.org}
     */
    class VideoKeyboardResponsePlugin {
        constructor(jsPsych) {
            this.jsPsych = jsPsych;
        }
        trial(display_element, trial) {
            // catch mistake where stimuli are not an array
            if (!Array.isArray(trial.stimulus)) {
                throw new Error(`
        The stimulus property for the video-keyboard-response plugin must be an array
        of files. See https://www.jspsych.org/latest/plugins/video-keyboard-response/#parameters
      `);
            }
            // setup stimulus
            //var video_html = "<div>";
            let video_html =
                '<div style="position:relative;">' +
                '<canvas id="canvas" width="' +
                trial.canvas_size[0] +
                '" height="' +
                trial.canvas_size[1] +
                '" style="border: ' +
                trial.canvas_border +
                '; position: absolute; z-index: -1; top: 50%; left: 50%; transform: translate(-50%, -50%);"></canvas>' +
                '<video id="jspsych-video-keyboard-response-stimulus"';
            ("</div>");

            //video_html += '<video id="jspsych-video-keyboard-response-stimulus"';
            if (trial.width) {
                video_html += ' width="' + trial.width + '"';
            }
            if (trial.height) {
                video_html += ' height="' + trial.height + '"';
            }
            if (trial.autoplay && trial.start == null) {
                // if autoplay is true and the start time is specified, then the video will start automatically
                // via the play() method, rather than the autoplay attribute, to prevent showing the first frame
                video_html += " autoplay ";
            }
            if (trial.controls) {
                video_html += " controls ";
            }
            if (trial.start !== null) {
                // hide video element when page loads if the start time is specified,
                // to prevent the video element from showing the first frame
                video_html += ' style="visibility: hidden;"';
            }
            video_html += ">";
            var video_preload_blob = this.jsPsych.pluginAPI.getVideoBuffer(trial.stimulus[0]);
            if (!video_preload_blob) {
                for (var i = 0; i < trial.stimulus.length; i++) {
                    var file_name = trial.stimulus[i];
                    if (file_name.indexOf("?") > -1) {
                        file_name = file_name.substring(0, file_name.indexOf("?"));
                    }
                    var type = file_name.substr(file_name.lastIndexOf(".") + 1);
                    type = type.toLowerCase();
                    if (type == "mov") {
                        console.warn("Warning: video-keyboard-response plugin does not reliably support .mov files.");
                    }
                    video_html += '<source src="' + file_name + '" type="video/' + type + '">';
                }
            }
            video_html += "</video>";
            video_html += "</div>";
            // add prompt if there is one
            if (trial.prompt !== null) {
                video_html += trial.prompt;
            }
            display_element.innerHTML = video_html;

            var video_element = display_element.querySelector("#jspsych-video-keyboard-response-stimulus");
            if (video_preload_blob) {
                video_element.src = video_preload_blob;
            }
            video_element.onended = () => {
                if (trial.trial_ends_after_video) {
                    end_trial();
                }
                if (trial.response_allowed_while_playing == false && !trial.trial_ends_after_video) {
                    // start keyboard listener
                    this.jsPsych.pluginAPI.getKeyboardResponse({
                        callback_function: after_response,
                        valid_responses: trial.choices,
                        rt_method: "performance",
                        persist: false,
                        allow_held_key: false,
                    });
                }
                video_element.style.visibility = "hidden";
            };
            video_element.playbackRate = trial.rate;
            // if video start time is specified, hide the video and set the starting time
            // before showing and playing, so that the video doesn't automatically show the first frame
            if (trial.start !== null) {
                video_element.pause();
                video_element.onseeked = () => {
                    video_element.style.visibility = "visible";
                    video_element.muted = false;
                    if (trial.autoplay) {
                        video_element.play();
                    } else {
                        video_element.pause();
                    }
                    video_element.onseeked = () => { };
                };
                video_element.onplaying = () => {
                    video_element.currentTime = trial.start;
                    video_element.onplaying = () => { };
                };
                // fix for iOS/MacOS browsers: videos aren't seekable until they start playing, so need to hide/mute, play,
                // change current time, then show/unmute
                video_element.muted = true;
                video_element.play();
            }
            let stopped = false;
            if (trial.stop !== null) {
                video_element.addEventListener("timeupdate", (e) => {
                    var currenttime = video_element.currentTime;
                    if (currenttime >= trial.stop) {
                        if (!trial.response_allowed_while_playing) {
                            this.jsPsych.pluginAPI.getKeyboardResponse({
                                callback_function: after_response,
                                valid_responses: trial.choices,
                                rt_method: "performance",
                                persist: false,
                                allow_held_key: false,
                            });
                        }
                        video_element.pause();
                        if (trial.trial_ends_after_video && !stopped) {
                            // this is to prevent end_trial from being called twice, because the timeupdate event
                            // can fire in quick succession
                            stopped = true;
                            end_trial();
                        }
                    }
                });
            }

            // Custom hacks to draw text above video and boxes below!
            let canvas = document.querySelector("canvas");
            let ctx = canvas.getContext("2d");
            ctx.fillStyle = trial.canvas_colour;
            ctx.fillRect(0, 0, trial.canvas_size[0], trial.canvas_size[1]);

            if (trial.translate_origin) {
                ctx.translate(canvas.width / 2, canvas.height / 2); // make center (0, 0)
            }

            // question
            ctx.textAlign = "center";
            ctx.textBaseline = "middle";
            ctx.font = trial.answer_font;
            ctx.fillStyle = trial.answer_colour;
            ctx.fillText(trial.answer_text, trial.answer_position[0], trial.answer_position[1]);

            // boxes colour
            ctx.fillStyle = trial.box_colour_left;
            ctx.fillRect(-trial.box_position[0] * 2, trial.box_position[1], trial.box_size, trial.box_size);
            ctx.fillStyle = trial.box_colour_right;
            ctx.fillRect(trial.box_position[0], trial.box_position[1], trial.box_size, trial.box_size);

            // boxes border
            ctx.beginPath();
            ctx.lineWidth = trial.box_frame;
            ctx.strokeStyle = "Black";
            ctx.rect(-trial.box_position[0] * 2, trial.box_position[1], trial.box_size, trial.box_size);
            ctx.stroke();

            // boxes border
            ctx.beginPath();
            ctx.lineWidth = trial.box_frame;
            ctx.strokeStyle = "Black";
            ctx.rect(trial.box_position[0], trial.box_position[1], trial.box_size, trial.box_size);
            ctx.stroke();

            // store response
            var response = {
                rt: null,
                key: null,
            };
            // function to end trial when it is time
            const end_trial = () => {
                // kill any remaining setTimeout handlers
                this.jsPsych.pluginAPI.clearAllTimeouts();
                // kill keyboard listeners
                this.jsPsych.pluginAPI.cancelAllKeyboardResponses();
                // stop the video file if it is playing
                // remove end event listeners if they exist
                display_element.querySelector("#jspsych-video-keyboard-response-stimulus").pause();
                display_element.querySelector("#jspsych-video-keyboard-response-stimulus").onended = () => { };
                // gather the data to store for the trial
                var trial_data = {
                    rt: response.rt,
                    stimulus: trial.stimulus,
                    response: response.key,
                };
                // clear the display
                display_element.innerHTML = "";
                // move on to the next trial
                this.jsPsych.finishTrial(trial_data);
            };
            // function to handle responses by the subject
            var after_response = (info) => {
                // after a valid response, the stimulus will have the CSS class 'responded'
                // which can be used to provide visual feedback that a response was recorded
                display_element.querySelector("#jspsych-video-keyboard-response-stimulus").className += " responded";
                // only record the first response
                if (response.key == null) {
                    response = info;
                }
                if (trial.response_ends_trial) {
                    end_trial();
                }
            };
            // start the response listener
            if (trial.choices != "NO_KEYS" && trial.response_allowed_while_playing) {
                this.jsPsych.pluginAPI.getKeyboardResponse({
                    callback_function: after_response,
                    valid_responses: trial.choices,
                    rt_method: "performance",
                    persist: false,
                    allow_held_key: false,
                });
            }
            // end trial if time limit is set
            if (trial.trial_duration !== null) {
                this.jsPsych.pluginAPI.setTimeout(end_trial, trial.trial_duration);
            }
        }
        simulate(trial, simulation_mode, simulation_options, load_callback) {
            if (simulation_mode == "data-only") {
                load_callback();
                this.simulate_data_only(trial, simulation_options);
            }
            if (simulation_mode == "visual") {
                this.simulate_visual(trial, simulation_options, load_callback);
            }
        }
        simulate_data_only(trial, simulation_options) {
            const data = this.create_simulation_data(trial, simulation_options);
            this.jsPsych.finishTrial(data);
        }
        simulate_visual(trial, simulation_options, load_callback) {
            const data = this.create_simulation_data(trial, simulation_options);
            const display_element = this.jsPsych.getDisplayElement();
            this.trial(display_element, trial);
            load_callback();
            const video_element = display_element.querySelector("#jspsych-video-button-response-stimulus");
            const respond = () => {
                if (data.rt !== null) {
                    this.jsPsych.pluginAPI.pressKey(data.response, data.rt);
                }
            };
            if (!trial.response_allowed_while_playing) {
                video_element.addEventListener("ended", respond);
            } else {
                respond();
            }
        }
        create_simulation_data(trial, simulation_options) {
            const default_data = {
                stimulus: trial.stimulus,
                rt: this.jsPsych.randomization.sampleExGaussian(500, 50, 1 / 150, true),
                response: this.jsPsych.pluginAPI.getValidKey(trial.choices),
            };
            const data = this.jsPsych.pluginAPI.mergeSimulationData(default_data, simulation_options);
            this.jsPsych.pluginAPI.ensureSimulationDataConsistency(trial, data);
            return data;
        }
    }
    VideoKeyboardResponsePlugin.info = info;

    return VideoKeyboardResponsePlugin;
})(jsPsychModule);
