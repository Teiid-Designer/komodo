/*
 * Copyright 2012 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.shell;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.List;
import java.util.Map.Entry;

import org.jboss.aesh.console.Console;
import org.jboss.aesh.console.ConsoleOutput;
import org.jboss.aesh.console.Prompt;
import org.jboss.aesh.console.settings.Settings;
import org.komodo.shell.api.ShellCommandFactory;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.utils.TextFormat;
import org.komodo.spi.utils.TextFormat.Rgb;
import org.komodo.utils.StringUtils;

/**
 * An implementation of the {@link ShellCommandReader} that uses JLine to provide
 * a rich console experience to the user, complete with history, tab completion,
 * and ansi output.
 *
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use WorkspaceStatus
 *
 * @author eric.wittmann@redhat.com
 */
public class InteractiveShellCommandReader extends AbstractShellCommandReader {

    private interface Ansi {

        String ESCAPE = "\033"; //$NON-NLS-1$
        String RESET = ESCAPE + "[0m "; //$NON-NLS-1$

        String BOLD = ESCAPE + "[1m"; //$NON-NLS-1$
        String ITALIC = ESCAPE + "[3m"; //$NON-NLS-1$
        String ITALIC_OFF = ESCAPE + "[23m"; //$NON-NLS-1$
        String UNDERLINE = ESCAPE + "[4m"; //$NON-NLS-1$
        String UNDERLINE_OFF = ESCAPE + "[24m"; //$NON-NLS-1$
        String NORMAL = ESCAPE + "[22m"; //$NON-NLS-1$

        String BLACK_FG = ESCAPE + "[30m"; //$NON-NLS-1$
        String RED_FG = ESCAPE + "[31m"; //$NON-NLS-1$
        String GREEN_FG = ESCAPE + "[32m"; //$NON-NLS-1$
        String YELLOW_FG = ESCAPE + "[33m"; //$NON-NLS-1$
        String BLUE_FG = ESCAPE + "[34m"; //$NON-NLS-1$
        String MAGENTA_FG = ESCAPE + "[35m"; //$NON-NLS-1$
        String CYAN_FG = ESCAPE + "[36m"; //$NON-NLS-1$
        String WHITE_FG = ESCAPE + "[37m"; //$NON-NLS-1$
        String DEFAULT_FG = ESCAPE + "[39m"; //$NON-NLS-1$

        String BLACK_BG = ESCAPE + "[40m"; //$NON-NLS-1$ // 0,0,0
        String RED_BG = ESCAPE + "[41m"; //$NON-NLS-1$ // 255,0,0
        String GREEN_BG = ESCAPE + "[42m"; //$NON-NLS-1$ // 0,255,0
        String YELLOW_BG = ESCAPE + "[43m"; //$NON-NLS-1$ // 255,255,0
        String BLUE_BG = ESCAPE + "[44m"; //$NON-NLS-1$ // 0,0,255
        String MAGENTA_BG = ESCAPE + "[45m"; //$NON-NLS-1$ // 255,0,255
        String CYAN_BG = ESCAPE + "[46m"; //$NON-NLS-1$ // 0,255,255
        String WHITE_BG = ESCAPE + "[47m"; //$NON-NLS-1$ // 255,255,255
        String DEFAULT_BG = ESCAPE + "[49m"; //$NON-NLS-1$

        String BG_PATTERN = ESCAPE + "[48;2;%d;%d;%dm"; //$NON-NLS-1$ // parameters are RGB values
        String FG_PATTERN = ESCAPE + "[38;2;%d;%d;%dm"; //$NON-NLS-1$ // parameters are RGB values
    }

    private Console consoleReader;

    /**
     * Constructor.
     * @param factory shell command factory
     * @param wsStatus the workspace status
     */
    public InteractiveShellCommandReader(ShellCommandFactory factory, WorkspaceStatus wsStatus) {
        super( factory, wsStatus );
    }

    /**
     * @see org.komodo.shell.AbstractShellCommandReader#open()
     */
    @Override
    public void open() throws Exception {
        Settings settings = Settings.getInstance();
        settings.setAliasEnabled( false );
        // settings.setAliasFile(new File("al"));
        settings.setEnablePipelineAndRedirectionParser( false );
        settings.setLogging( true );

        consoleReader = new Console( settings );
        consoleReader.addCompletion( new TabCompleter( getFactory() ) );
    }

    /**
     * Creates the ANSI compatible prompt.
     * @throws Exception
     */
    private String defaultAnsiPrompt() throws Exception {
        final StringBuilder builder = new StringBuilder();
        final List< Entry< TextFormat, String > > prompt = getPrompt();

        for ( final Entry< TextFormat, String > segment : prompt ) {
            final TextFormat format = segment.getKey();

            if ( format.isBold() ) {
                builder.append( Ansi.BOLD );
            }

            if ( format.isItalic() ) {
                builder.append( Ansi.ITALIC );
            }

            if ( format.isUnderline() ) {
                builder.append( Ansi.UNDERLINE );
            }

            builder.append( getBackgroundColor( format.getBackground() ) );
            builder.append( getForegroundColor( format.getForeground() ) );

            { // add prompt text
                final String text = segment.getValue();
                String displayPrompt = text;

                switch ( format.getTextCase() ) {
                    case UPPER:
                        displayPrompt = text.toUpperCase();
                        break;
                    case LOWER:
                        displayPrompt = text.toLowerCase();
                        break;
                    case CAMEL:
                        displayPrompt = StringUtils.toCamelCase( text );
                        break;
                    case LOWER_CAMEL:
                        displayPrompt = StringUtils.toLowerCamelCase( text );
                        break;
                    case AS_IS:
                    default:
                        break;
                }

                builder.append( displayPrompt );
            }

            if ( format.isBold() ) {
                builder.append( Ansi.NORMAL );
            }

            if ( format.isItalic() ) {
                builder.append( Ansi.ITALIC_OFF );
            }

            if ( format.isUnderline() ) {
                builder.append( Ansi.UNDERLINE_OFF );
            }
        }
        builder.append( Ansi.RESET );
        return builder.toString();
    }

    private String getBackgroundColor( final Rgb background ) {
        if ( background == null ) {
            return Ansi.DEFAULT_BG;
        }

        if ( Rgb.BLACK == background ) {
            return Ansi.BLACK_BG;
        }

        if ( Rgb.RED == background ) {
            return Ansi.RED_BG;
        }

        if ( Rgb.GREEN == background ) {
            return Ansi.GREEN_BG;
        }

        if ( Rgb.YELLOW == background ) {
            return Ansi.YELLOW_BG;
        }

        if ( Rgb.BLUE == background ) {
            return Ansi.BLUE_BG;
        }

        if ( Rgb.MAGENTA == background ) {
            return Ansi.MAGENTA_BG;
        }

        if ( Rgb.CYAN == background ) {
            return Ansi.CYAN_BG;
        }

        if ( Rgb.WHITE == background ) {
            return Ansi.WHITE_BG;
        }

        return String.format( Ansi.BG_PATTERN, background.getRed(), background.getGreen(), background.getBlue() );
    }

    private String getForegroundColor( final Rgb foreground ) {
        if ( foreground == null ) {
            return Ansi.DEFAULT_FG;
        }

        if ( Rgb.BLACK == foreground ) {
            return Ansi.BLACK_FG;
        }

        if ( Rgb.RED == foreground ) {
            return Ansi.RED_FG;
        }

        if ( Rgb.GREEN == foreground ) {
            return Ansi.GREEN_FG;
        }

        if ( Rgb.YELLOW == foreground ) {
            return Ansi.YELLOW_FG;
        }

        if ( Rgb.BLUE == foreground ) {
            return Ansi.BLUE_FG;
        }

        if ( Rgb.MAGENTA == foreground ) {
            return Ansi.MAGENTA_FG;
        }

        if ( Rgb.CYAN == foreground ) {
            return Ansi.CYAN_FG;
        }

        if ( Rgb.WHITE == foreground ) {
            return Ansi.WHITE_FG;
        }

        return String.format( Ansi.FG_PATTERN, foreground.getRed(), foreground.getGreen(), foreground.getBlue() );
    }

    private Prompt doGetPrompt() throws Exception {
        return new Prompt( defaultAnsiPrompt() );
    }

    /**
     * @see org.komodo.common.shell.AbstractShellCommandReader#readLine()
     */
    @Override
    protected String readLine() throws IOException {
        try {
            final ConsoleOutput output = consoleReader.read( doGetPrompt(), null );
            return output.getBuffer();
        } catch ( Exception ex ) {
            throw new IOException( ex );
        }
    }

    /**
     * @see org.komodo.common.shell.AbstractShellCommandReader#getOutputWriter()
     */
    @Override
    protected Writer getOutputWriter() {
        return new OutputStreamWriter( Settings.getInstance().getStdOut() );
    }

    /**
     * @see org.komodo.shell.ShellCommandReader#close()
     */
    @Override
    public void close() throws IOException {
        consoleReader.stop();
    }

    /**
     * @see org.komodo.shell.ShellCommandReader#promptForInput(java.lang.String)
     */
    @Override
    public String promptForInput( String promptString ) {
        try {
            return this.consoleReader.read( doGetPrompt().getPromptAsString() ).getBuffer();
        } catch ( Exception e ) {
            throw new RuntimeException( e );
        }
    }

    /**
     * @see org.komodo.shell.ShellCommandReader#promptForPassword(java.lang.String)
     */
    @Override
    public String promptForPassword( String promptString ) {
        try {
            Prompt newPrompt = doGetPrompt();
            return this.consoleReader.read( newPrompt, Character.valueOf( ( char )0 ) ).getBuffer();
        } catch ( Exception e ) {
            throw new RuntimeException( e );
        }
    }

}
