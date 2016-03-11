/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.spi.utils;

import java.util.BitSet;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Text format settings.
 */
public class TextFormat {

    /**
     * Text alignment setting.
     */
    public enum Alignment {

        /**
         * Text should not be aligned.
         */
        AS_IS,

        /**
         * Text should be block aligned.
         */
        BLOCK,

        /**
         * Text should be center aligned.
         */
        CENTER,

        /**
         * Text should be left aligned.
         */
        LEFT,

        /**
         * Text should be right aligned.
         */
        RIGHT;

        static final Alignment DEFAULT = AS_IS;

    }

    /**
     * Represents a RGB color value.
     */
    public static class Rgb {

        /**
         * The color black.
         */
        public final static Rgb BLACK = new Rgb( 0, 0, 0 );

        /**
         * The color blue.
         */
        public final static Rgb BLUE = new Rgb( 0, 0, 255 );

        /**
         * The color cyan.
         */
        public final static Rgb CYAN = new Rgb( 0, 255, 255 );

        /**
         * The color dark gray.
         */
        public final static Rgb DARK_GRAY = new Rgb( 64, 64, 64 );

        /**
         * The color gray.
         */
        public final static Rgb GRAY = new Rgb( 128, 128, 128 );

        /**
         * The color green.
         */
        public final static Rgb GREEN = new Rgb( 0, 255, 0 );

        /**
         * The color light gray.
         */
        public final static Rgb LIGHT_GRAY = new Rgb( 192, 192, 192 );

        /**
         * The color magenta.
         */
        public final static Rgb MAGENTA = new Rgb( 255, 0, 255 );

        /**
         * The color orange.
         */
        public final static Rgb ORANGE = new Rgb( 255, 200, 0 );

        /**
         * The color pink.
         */
        public final static Rgb PINK = new Rgb( 255, 175, 175 );

        /**
         * The color red.
         */
        public final static Rgb RED = new Rgb( 255, 0, 0 );

        /**
         * The color white.
         */
        public final static Rgb WHITE = new Rgb( 255, 255, 255 );

        /**
         * The color yellow.
         */
        public final static Rgb YELLOW = new Rgb( 255, 255, 0 );

        private final int blue;
        private final int green;
        private final int red;

        /**
         * @param redComponent
         *        the red component (if less than zero is set to zero)
         * @param greenComponent
         *        the green component (if less than zero is set to zero)
         * @param blueComponent
         *        the blue component (if less than zero is set to zero)
         */
        public Rgb( final int redComponent,
                    final int greenComponent,
                    final int blueComponent ) {
            this.red = ( ( redComponent < 0 ) ? 0 : redComponent );
            this.green = ( ( greenComponent < 0 ) ? 0 : greenComponent );
            this.blue = ( ( blueComponent < 0 ) ? 0 : blueComponent );
        }

        /**
         * @return the blue component
         */
        public int getBlue() {
            return this.red;
        }

        /**
         * @return the green component
         */
        public int getGreen() {
            return this.red;
        }

        /**
         * @return the red component
         */
        public int getRed() {
            return this.red;
        }

        /**
         * {@inheritDoc}
         *
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "RGB(" + this.red + ", " + this.green + ", " + this.blue + ')'; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }

    }

    /**
     * Text case settings.
     */
    public enum TextCase {

        /**
         * Text case should not be changed.
         */
        AS_IS,

        /**
         * Text case should be upper camel case (first letter of every word should be capitalized).
         */
        CAMEL,

        /**
         * Text case should be lower case.
         */
        LOWER,

        /**
         * Text case should be lower camel case (first letter of every word but the first word should be capitalized).
         */
        LOWER_CAMEL,

        /**
         * Text case should be upper case.
         */
        UPPER;

        static final TextCase DEFAULT = AS_IS;

    }

    /**
     * Indicates that the default text size should be used.
     */
    public static final int DEFAULT_SIZE = -1;

    private static final int BOLD = 0;
    private static final int ITALIC = 1;
    private static final int UNDERLINE = 2;

    private Alignment alignment;
    private Rgb bgColor;
    private Rgb fgColor;
    private final Map< String, Object > props;
    private int size;
    private final BitSet style;
    private TextCase textCase;

    /**
     * Constructs a default text format.
     */
    public TextFormat() {
        this.props = new HashMap< >();
        this.style = new BitSet( 3 );
    }

    /**
     * Make the text bold.
     */
    public void bold() {
        this.style.set( BOLD );
    }

    /**
     * @return the text alignment (never <code>null</code>)
     */
    public Alignment getAlignment() {
        return ( ( this.alignment == null ) ? Alignment.DEFAULT : this.alignment );
    }

    /**
     * @return the background color (can be <code>null</code> if the default color should be used)
     */
    public Rgb getBackground() {
        return this.bgColor;
    }

    /**
     * @param propName
     *        the name of the custom property (can be empty)
     * @return the property value or <code>null</code> if not found or if the property name is empty
     */
    public Object getCustomPropert( final String propName ) {
        if ( ( propName != null ) && !propName.isEmpty() ) {
            return this.props.get( propName );
        }

        return null;
    }

    /**
     * @return an unmodifiable map of the custom properties (never <code>null</code> but can be empty)
     */
    public Map< String, Object > getCustomProperties() {
        return Collections.unmodifiableMap( this.props );
    }

    /**
     * @return the foreground color (can be <code>null</code> if the default color should be used)
     */
    public Rgb getForeground() {
        return this.fgColor;
    }

    /**
     * @return the text size or {@value #DEFAULT_SIZE if the default size should be used}
     */
    public int getSize() {
        if ( ( this.size == 0 ) || ( this.size < DEFAULT_SIZE ) ) {
            return DEFAULT_SIZE;
        }

        return this.size;
    }

    /**
     * @return the text case (never <code>null</code>)
     */
    public TextCase getTextCase() {
        return ( ( this.textCase == null ) ? TextCase.DEFAULT : this.textCase );
    }

    /**
     * @return <code>true</code> if the style is bold
     */
    public boolean isBold() {
        return this.style.get( BOLD );
    }

    /**
     * @return <code>true</code> if the style is italic
     */
    public boolean isItalic() {
        return this.style.get( ITALIC );
    }

    /**
     * @return <code>true</code> if the style is underline
     */
    public boolean isUnderline() {
        return this.style.get( UNDERLINE );
    }

    /**
     * Make the text italic.
     */
    public void italic() {
        this.style.set( ITALIC );
    }

    /**
     * Resets to the default alignment, size, style, and case.
     */
    public void reset() {
        this.alignment = Alignment.DEFAULT;
        this.bgColor = null;
        this.fgColor = null;
        this.size = DEFAULT_SIZE;
        this.style.clear();
        this.textCase = TextCase.DEFAULT;
    }

    /**
     * @param newAlignment
     *        the new alignment (can be <code>null</code>)
     */
    public void setAlignment( final Alignment newAlignment ) {
        this.alignment = newAlignment;
    }

    /**
     * @param newBackground
     *        the new background color (can be <code>null</code>)
     */
    public void setBackground( final Rgb newBackground ) {
        this.bgColor = newBackground;
    }

    /**
     * A <code>null</code> value will remove the property; while a <code>null</code> property name does nothing.
     *
     * @param propName
     *        the name of the custom property (can be empty)
     * @param value
     *        the property value (can be <code>null</code>)
     */
    public void setCustomProperty( final String propName,
                                   final Object value ) {
        if ( ( propName != null ) && !propName.isEmpty() ) {
            if ( value == null ) {
                this.props.remove( propName );
            } else {
                this.props.put( propName, value );
            }
        }
    }

    /**
     * @param newForeground
     *        the new foreground color (can be <code>null</code>)
     */
    public void setForeground( final Rgb newForeground ) {
        this.fgColor = newForeground;
    }

    /**
     * @param newTextCase
     *        the new case (can be <code>null</code>)
     */
    public void setTextCase( final TextCase newTextCase ) {
        this.textCase = newTextCase;
    }

    /**
     * Make the text not bold.
     */
    public void unBold() {
        this.style.clear( BOLD );
    }

    /**
     * Make the text underlined.
     */
    public void underline() {
        this.style.set( UNDERLINE );
    }

    /**
     * Make the text not italic.
     */
    public void unItalic() {
        this.style.clear( ITALIC );
    }

    /**
     * Make the text not underlined.
     */
    public void unUnderline() {
        this.style.clear( UNDERLINE );
    }

}
